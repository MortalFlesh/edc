#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open System

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools.Git

type ToolDir =
    /// Global tool dir must be in PATH - ${PATH}:/root/.dotnet/tools
    | Global
    /// Just a dir name, the location will be used as: ./{LocalDirName}
    | Local of string

// ========================================================================================================
// === F# / SAFE app fake build =================================================================== 1.0.0 =
// --------------------------------------------------------------------------------------------------------
// Options:
//  - no-lint    - lint will be executed, but the result is not validated
// --------------------------------------------------------------------------------------------------------
// Table of contents:
//      1. Information about project, configuration
//      2. Utilities, DotnetCore functions
//      3. FAKE targets
//      4. FAKE targets hierarchy
// ========================================================================================================

// --------------------------------------------------------------------------------------------------------
// 1. Information about the project to be used at NuGet and in AssemblyInfo files and other FAKE configuration
// --------------------------------------------------------------------------------------------------------

let project = "MF.EDC"
let summary = "GUI for configuration of EDC sets"

let release = ReleaseNotes.parse (System.IO.File.ReadAllLines "RELEASE_NOTES.md" |> Seq.filter ((<>) "## Unreleased"))
let gitCommit = Information.getCurrentSHA1(".")
let gitBranch = Information.getBranchName(".")

let toolsDir = Global

Target.initEnvironment ()

let serverPath = Path.getFullName "./src/Server"
let clientPath = Path.getFullName "./src/Client"
let clientDeployPath = Path.combine clientPath "deploy"
let deployDir = Path.getFullName "./deploy"

// --------------------------------------------------------------------------------------------------------
// 2. Utilities, DotnetCore functions, etc.
// --------------------------------------------------------------------------------------------------------

let platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | _ ->
        let errorMsg =
            tool + " was not found in path. " +
            "Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        failwith errorMsg

let nodeTool = platformTool "node" "node.exe"
let npmTool = platformTool "npm" "npm.cmd"
let npxTool = platformTool "npx" "npx.cmd"

let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let openBrowser url =
    //https://github.com/dotnet/corefx/issues/10361
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCodeWithMessage "opening browser failed"
    |> Proc.run
    |> ignore

[<AutoOpen>]
module private Utils =
    let tee f a =
        f a
        a

    let skipOn option action p =
        if p.Context.Arguments |> Seq.contains option
        then Trace.tracefn "Skipped ..."
        else action p

module private DotnetCore =
    let run cmd workingDir =
        let options =
            DotNet.Options.withWorkingDirectory workingDir
            >> DotNet.Options.withRedirectOutput true

        DotNet.exec options cmd ""

    let runOrFail cmd workingDir =
        run cmd workingDir
        |> tee (fun result ->
            if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir
        )
        |> ignore

    let runInRoot cmd = run cmd "."
    let runInRootOrFail cmd = runOrFail cmd "."

    let installOrUpdateTool toolDir tool =
        let toolCommand action =
            match toolDir with
            | Global -> sprintf "tool %s --global %s" action tool
            | Local dir -> sprintf "tool %s --tool-path ./%s %s" action dir tool

        match runInRoot (toolCommand "install") with
        | { ExitCode = code } when code <> 0 ->
            match runInRoot (toolCommand "update") with
            | { ExitCode = code } when code <> 0 -> Trace.tracefn "Warning: Install and update of %A has failed." tool
            | _ -> ()
        | _ -> ()

    let execute command args (dir: string) =
        let cmd =
            sprintf "%s/%s"
                (dir.TrimEnd('/'))
                command

        let processInfo = System.Diagnostics.ProcessStartInfo(cmd)
        processInfo.RedirectStandardOutput <- true
        processInfo.RedirectStandardError <- true
        processInfo.UseShellExecute <- false
        processInfo.CreateNoWindow <- true
        processInfo.Arguments <- args |> String.concat " "

        use proc =
            new System.Diagnostics.Process(
                StartInfo = processInfo
            )
        if proc.Start() |> not then failwith "Process was not started."
        proc.WaitForExit()

        if proc.ExitCode <> 0 then failwithf "Command '%s' failed in %s." command dir
        (proc.StandardOutput.ReadToEnd(), proc.StandardError.ReadToEnd())

let envVar name =
    if Environment.hasEnvironVar(name)
        then Environment.environVar(name) |> Some
        else None

let stringToOption = function
    | null | "" -> None
    | string -> Some string

[<RequireQualifiedAccess>]
module Option =
    let mapNone f = function
        | Some v -> v
        | None -> f None

    let bindNone f = function
        | Some v -> Some v
        | None -> f None

// --------------------------------------------------------------------------------------------------------
// 3. Targets for FAKE
// --------------------------------------------------------------------------------------------------------

Target.create "Clean" (fun _ ->
    !! "./**/bin/Release"
    ++ "./**/bin/Debug"
    ++ "./**/obj"
    ++ "./**/.ionide"
    |> Shell.cleanDirs
)

Target.create "SafeClean" (fun _ ->
    [ deployDir
      clientDeployPath ]
    |> Shell.cleanDirs
)

Target.create "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        let now = DateTime.Now

        let gitValue fallbackEnvironmentVariableNames initialValue =
            initialValue
            |> String.replace "NoBranch" ""
            |> stringToOption
            |> Option.bindNone (fun _ -> fallbackEnvironmentVariableNames |> List.tryPick envVar)
            |> Option.defaultValue "unknown"

        [
            AssemblyInfo.Title projectName
            AssemblyInfo.Product project
            AssemblyInfo.Description summary
            AssemblyInfo.Version release.AssemblyVersion
            AssemblyInfo.FileVersion release.AssemblyVersion
            AssemblyInfo.InternalsVisibleTo "tests"
            AssemblyInfo.Metadata("gitbranch", gitBranch |> gitValue [ "GIT_BRANCH"; "branch" ])
            AssemblyInfo.Metadata("gitcommit", gitCommit |> gitValue [ "GIT_COMMIT"; "commit" ])
            AssemblyInfo.Metadata("buildNumber", "BUILD_NUMBER" |> envVar |> Option.defaultValue "-")
            AssemblyInfo.Metadata("createdAt", now.ToString("yyyy-MM-dd HH:mm:ss"))
            AssemblyInfo.Metadata("SafeTemplateVersion", "1.19.0")
        ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        (
            projectPath,
            projectName,
            System.IO.Path.GetDirectoryName(projectPath),
            (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.fsproj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (_, _, folderName, attributes) ->
        AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
    )
)

Target.create "InstallClient" (fun _ ->
    printfn "Node version:"
    runTool nodeTool "--version" __SOURCE_DIRECTORY__
    printfn "Npm version:"
    runTool npmTool "--version"  __SOURCE_DIRECTORY__
    runTool npmTool "install" __SOURCE_DIRECTORY__
)

Target.create "Build" (fun _ ->
    serverPath |> DotnetCore.runOrFail "build"

    runTool npxTool "webpack-cli -p" __SOURCE_DIRECTORY__
)

Target.create "Lint" <| skipOn "no-lint" (fun _ ->
    DotnetCore.installOrUpdateTool toolsDir "dotnet-fsharplint"

    let checkResult (messages: string list) =
        let rec check: string list -> unit = function
            | [] -> failwithf "Lint does not yield a summary."
            | head :: rest ->
                if head.Contains "Summary" then
                    match head.Replace("= ", "").Replace(" =", "").Replace("=", "").Replace("Summary: ", "") with
                    | "0 warnings" -> Trace.tracefn "Lint: OK"
                    | warnings -> failwithf "Lint ends up with %s." warnings
                else check rest
        messages
        |> List.rev
        |> check

    !! "src/**/*.fsproj"
    |> Seq.map (fun fsproj ->
        match toolsDir with
        | Global ->
            DotnetCore.runInRoot (sprintf "fsharplint lint %s" fsproj)
            |> fun (result: ProcessResult) -> result.Messages
        | Local dir ->
            DotnetCore.execute "dotnet-fsharplint" ["lint"; fsproj] dir
            |> fst
            |> tee (Trace.tracefn "%s")
            |> String.split '\n'
            |> Seq.toList
    )
    |> Seq.iter checkResult
)

Target.create "Run" (fun _ ->
    let server = async {
        serverPath |> DotnetCore.runOrFail "watch run"
    }
    let client = async {
        runTool npxTool "webpack-dev-server" __SOURCE_DIRECTORY__
    }
    let browser = async {
        do! Async.Sleep 10000
        openBrowser "http://127.0.0.1:8080"
    }

    let vsCodeSession = Environment.hasEnvironVar "vsCodeSession"
    let safeClientOnly = Environment.hasEnvironVar "safeClientOnly"

    let tasks =
        [ if not safeClientOnly then yield server
          yield client
          if not vsCodeSession then yield browser ]

    tasks
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
)

Target.create "Bundle" (fun _ ->
    let serverDir = deployDir </> "Server"
    let clientDir = deployDir </> "Client"
    let publicDir = clientDir </> "public"

    serverPath |> DotnetCore.runOrFail (sprintf "publish -c Release -o \"%s\"" serverDir)

    Shell.copyDir publicDir clientDeployPath FileFilter.allFiles
)

// --------------------------------------------------------------------------------------------------------
// 4. FAKE targets hierarchy
// --------------------------------------------------------------------------------------------------------

open Fake.Core.TargetOperators

"SafeClean"
    ==> "AssemblyInfo"
    ==> "InstallClient"
    ==> "Build"
    ==> "Lint"
    ==> "Bundle"

"SafeClean"
    ==> "InstallClient"
    ==> "Run"

Target.runOrDefaultWithArguments "Build"
