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

let publicPath = Path.GetFullPath "../Client/public"

let currentApplication =
    [ ".env"; ".dist.env" ]
    |> CurrentApplication.fromEnvironment
    |> Result.orFail

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

    let edc: IEdcApi = {
        //
        // Public actions
        //

        Login = fun credentials -> asyncResult {
            let! credentials =
                credentials
                |> Dto.Deserialize.credentials
                |> AsyncResult.ofResult <@> (CredentialsError.format >> ErrorMessage)

            let! user =
                ErrorMessage "Not implemented yet" |> AsyncResult.ofError

            return user |> Dto.Serialize.user
        }

        LoadProfiler = fun token -> async {
            return
                match currentApplication.Debug with
                | Dev -> Profiler.init currentApplication.Instance (sprintf "%A" Dev) |> Some
                | Prod when token = Some Profiler.token -> Profiler.init currentApplication.Instance (sprintf "%A with token" Prod) |> Some
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
    }

//
// Application
//

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
    use_gzip
}

run app
