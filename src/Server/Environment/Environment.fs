module Environment

open System
open System.Text.RegularExpressions
open ErrorHandling

// inspired by http://www.codesuji.com/2018/02/28/F-and-DotEnv/
let loadFromFile filePath =
    result {
        if IO.File.Exists(filePath) |> not then
            return! Error (sprintf "File %s does not exists." filePath)

        IO.File.ReadAllLines(filePath)
        |> Seq.iter (fun line ->
            let lineWithoutComments = Regex.Replace(line, "#.*", "")
            let parts =
                lineWithoutComments.Split('=')
                |> Array.map (fun x -> x.Trim())

            match parts with
            | [| key; value |] when String.IsNullOrWhiteSpace(Environment.GetEnvironmentVariable(key)) ->
                Environment.SetEnvironmentVariable(key, value.Trim([|'"'; '''|]))
            | _ -> ()
        )
    }

let getEnvs () =
    // https://stackoverflow.com/questions/12014179/f-get-environment-variables-as-a-generic-collection
    System.Environment.GetEnvironmentVariables()
    |> Seq.cast<System.Collections.DictionaryEntry>
    |> Seq.map (fun d -> d.Key :?> string, d.Value :?> string)
    |> Map

let private combine onSame (currentEnvs: Map<string, string>) (newEnvs: Map<string, string>): Map<string, string> =
    newEnvs
    |> Map.fold (fun acc key newValue ->
        match acc.TryFind key with
        | Some currentValue -> acc.Add(key, onSame currentValue newValue)
        | _ -> acc.Add(key, newValue)
    ) currentEnvs

let merge currentEnvs newEnvs =
    combine (fun currentValue _newValue -> currentValue) currentEnvs newEnvs

let update currentEnvs newEnvs =
    combine (fun _currentValue newValue -> newValue) currentEnvs newEnvs
