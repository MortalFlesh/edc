open System.IO
open System.Threading.Tasks

open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn

open ErrorHandling

open Shared
open MF.EDC
open MF.EDC.Profiler

open Fable.Remoting.Server
open Fable.Remoting.Giraffe

open Microsoft.WindowsAzure.Storage

let currentApplication =
    [ ".env"; ".dist.env" ]
    |> CurrentApplication.fromEnvironment
    // todo |> Result.orFail? or maybe CurrentApplication.defaults
    |> function
        | Ok app -> app
        | Error _ ->
            let tokenKey = JWTKey.forDevelopment
            {
                Instance = Instance.create "mf-edc-default"
                TokenKey = tokenKey
                KeysForToken = [
                    tokenKey
                ]
                Debug = Dev
                Dependencies = NoDependenciesYet
            }

// todo - move following to current application
let tryGetEnv key =
    Environment.getEnvs() |> Map.tryFind key

let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse

//
// Api
//

[<RequireQualifiedAccess>]
module Api =
    open ErrorHandling.AsyncResult.Operators
    open MF.EDC.Query

    let edc logMessage: IEdcApi =
        let logError = logMessage

        let inline (>?>) authorize action =
            Authorize.authorizeAction
                currentApplication.Instance
                currentApplication.TokenKey
                currentApplication.KeysForToken
                logError
                authorize
                action

        {
            //
            // Public actions
            //

            Login = fun credentials -> asyncResult {
                let! credentials =
                    credentials
                    |> Dto.Deserialize.credentials
                    |> AsyncResult.ofResult <@> (CredentialsError.format >> ErrorMessage)

                let user = {
                    Username = credentials.Username
                    Token =
                        JWTToken.create
                            currentApplication.Instance
                            currentApplication.TokenKey
                            [
                                CustomItem.String (UserCustomData.Username, credentials.Username |> Username.value)
                                CustomItem.String (UserCustomData.DisplayName, credentials.Username |> Username.value)
                                CustomItem.Strings (UserCustomData.Groups, [])
                            ]
                }

                return user |> Dto.Serialize.user
            }

            LoadProfiler = fun token -> async {
                return
                    match currentApplication.Debug with
                    | Dev -> Profiler.init currentApplication.Instance (Environment.getEnvs()) (sprintf "%A" Dev) |> Some
                    | Prod when token = Some Profiler.token -> Profiler.init currentApplication.Instance (Environment.getEnvs()) (sprintf "%A with token" Prod) |> Some
                    | Prod -> None
            }

            //
            // Secured actions
            //

            (*
                Example:

            LoadData = Authorize.withLogin >?> fun requestData -> asyncResult {
                let! data =
                    searchData requestData <@> (SearchError.format >> ErrorMessage)

                return data |> List.map Dto.Serialize.data
            }
            *)

            LoadItems = Authorize.withLogin >?> fun () -> asyncResult {
                let! data =
                    ItemsQuery.load <@> (ItemsError.format >> ErrorMessage)

                PersonsQuery.init()
                let! persons =
                    PersonsQuery.load <@> (PersonError.format >> ErrorMessage)

                printfn "Persons: %A" persons

                return data |> List.map Dto.Serialize.itemEntity
            }
    }

//
// Application
//

[<RequireQualifiedAccess>]
module AppInsightTelemetry =
    let configure (services: IServiceCollection) =
        tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
        |> Option.map services.AddApplicationInsightsTelemetry
        |> Option.defaultValue services

[<RequireQualifiedAccess>]
module Logger =
    open Microsoft.Extensions.Logging

    let logMessage (logger: ILogger) (message: string) =
        logger.LogInformation(sprintf "[Information] %s" message)
        logger.LogDebug(sprintf "[Debug] %s" message)
        logger.LogTrace(sprintf "[Trace] %s" message)
        logger.LogWarning(sprintf "[Warning] %s" message)
        logger.LogError(sprintf "[Error] %s" message)
        logger.LogCritical(sprintf "[Critical] %s" message)

        ()

    let configure (services: IServiceCollection) =
        services.AddLogging()

let apiRouter =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromContext (fun ctx ->
        // https://zaid-ajaj.github.io/Fable.Remoting/src/dependency-injection.html
        let logger = ctx.GetLogger<IEdcApi>()

        Api.edc (Logger.logMessage logger)  // todo - create ApplicationLogger
    )
    |> Remoting.buildHttpHandler

let appRouter = router {
    // get "/custom" (warbler (customPage >> text))

    getf "/.well-known/acme-challenge/%s" (fun challenge ->
        warbler (fun ctx ->
            "... todo - load from db ..." |> text
        )
    )

    forward "/api/" apiRouter
}

let app = application {
    url ("http://0.0.0.0:8085/")
    use_router appRouter
    memory_cache
    use_static publicPath
    service_config (
        AppInsightTelemetry.configure
        >> Logger.configure
    )
    use_gzip
}

run app
