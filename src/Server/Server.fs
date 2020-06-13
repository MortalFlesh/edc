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

open Microsoft.Azure.Cosmos.Table

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
                ProfilerToken = Profiler.Token "... todo ..."
                Dependencies = {
                    MysqlDatabase = MySqlConnectionString (ConnectionString "... todo ...")
                    AzureSqlDatabase = AzureSqlConnectionString (ConnectionString "... todo ...")
                }
            }

// todo - move following to current application
let envs =
    Environment.getEnvs()
    //|> tee (Map.toList >> List.iter (printfn "- %A"))
let tryGetEnv key =
    envs |> Map.tryFind key

let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse

//
// Api
//

[<RequireQualifiedAccess>]
module Api =
    open ErrorHandling.AsyncResult.Operators
    open MF.EDC.Query

    let edc logError: IEdcApi =
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
                    | Prod when token = Some currentApplication.ProfilerToken -> Profiler.init currentApplication.Instance (Environment.getEnvs()) (sprintf "%A with token" Prod) |> Some
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
                let mySqlLocalConnection = currentApplication.Dependencies.MysqlDatabase |> Database.MySql.MySql.connect
                let azureSqlConnection = currentApplication.Dependencies.AzureSqlDatabase |> Database.AzureSql.AzureSql.connect

                let! data =
                    ItemsQuery.load storageAccount mySqlLocalConnection azureSqlConnection <@> (ItemsError.format >> ErrorMessage)

                return data |> List.map Dto.Serialize.itemEntity
            }

            CreateItem = Authorize.withLogin >?> fun item -> asyncResult {
                // todo - deserialize item
                let fItem = item |> FlatItem.FlatItem.ofItem |> FlatItem.FlatItem.data
                let itemEntity =
                    {
                        Id = Id.create()
                        Item = Item.Tool (Knife {
                            Common = {
                                Name = fItem.Common.Name
                                Note = None
                                Color = None
                                Tags = []
                                Links = []
                                Price = None
                                Size = None
                                OwnershipStatus = Own
                                Product = None
                                Gallery = None
                            }
                        })
                    }

                let mysqlLocalConnection = currentApplication.Dependencies.MysqlDatabase |> Database.MySql.MySql.connect
                let azureSqlConnection = currentApplication.Dependencies.AzureSqlDatabase |> Database.AzureSql.AzureSql.connect

                let! _ =
                    itemEntity
                    |> Command.ItemsCommand.create storageAccount mysqlLocalConnection azureSqlConnection  <@> ErrorMessage

                return itemEntity.Item |> Dto.Serialize.item
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
        let logError = Logger.logMessage logger

        //let logError = eprintf "Error %s"

        Api.edc logError  // todo - create ApplicationLogger
    )
    //|> Remoting.fromValue (Api.edc (eprintf "%s"))
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
