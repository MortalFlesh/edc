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

let tryGetEnv key =
    Environment.getEnvs() |> Map.tryFind key

let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse

//
// Api
//

[<RequireQualifiedAccess>]
module Api =
    //let private logger = currentApplication.Logger.Error "EDC"
    let private logError = eprintfn "[EDC] %A"

    let inline private (>?>) authorize action =
        Authorize.authorizeAction
            currentApplication.Instance
            currentApplication.TokenKey
            currentApplication.KeysForToken
            logError
            authorize
            action

    open ErrorHandling.AsyncResult.Operators

    open MF.EDC.Query

    let edc: IEdcApi = {
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

            return data |> List.map Dto.Serialize.itemEntity
        }
    }

//
// Application
//

let configureAzure (services:IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services

let apiRouter =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue Api.edc
    |> Remoting.buildHttpHandler

let appRouter = router {
    // get "/custom" (warbler (customPage >> text))

    forward "/api/" apiRouter
}

let app = application {
    url ("http://0.0.0.0:8085/")
    use_router appRouter
    memory_cache
    use_static publicPath
    service_config configureAzure
    use_gzip
}

run app
