open ErrorHandling

open Shared
open MF.EDC

//
// Api
//

[<RequireQualifiedAccess>]
module Api =
    open ErrorHandling.AsyncResult.Operators
    open MF.EDC.Query
    open MF.EDC.Profiler

    let edc logError currentApplication: IEdcApi =  // todo - logError could be used from currentApplication.Logger
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
                    ItemsQuery.load currentApplication.Dependencies.StorageAccount mySqlLocalConnection azureSqlConnection <@> (ItemsError.format >> ErrorMessage)

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
                    |> Command.ItemsCommand.create currentApplication.Dependencies.StorageAccount mysqlLocalConnection azureSqlConnection  <@> ErrorMessage

                return itemEntity.Item |> Dto.Serialize.item
            }
    }

//
// Application
//

[<RequireQualifiedAccess>]
module WebApp =
    open Microsoft.Extensions.DependencyInjection

    open Giraffe
    open Saturn

    open Fable.Remoting.Server
    open Fable.Remoting.Giraffe

    [<RequireQualifiedAccess>]
    module AppInsightTelemetry =
        let configure currentApplication (services: IServiceCollection) =
            currentApplication.AppInsightKey
            |> Option.map (AppInsightKey.value >> services.AddApplicationInsightsTelemetry)
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

    let run currentApplication =
        let apiRouter =
            Remoting.createApi()
            |> Remoting.withRouteBuilder Route.builder
            |> Remoting.fromContext (fun ctx ->
                // https://zaid-ajaj.github.io/Fable.Remoting/src/dependency-injection.html
                let logger = ctx.GetLogger<IEdcApi>()
                let logError = Logger.logMessage logger

                Api.edc logError currentApplication  // todo - create ApplicationLogger
            )
            |> Remoting.buildHttpHandler

        let appRouter = router {
            // get "/custom" (warbler (customPage >> text))

            (* getf "/.well-known/acme-challenge/%s" (fun challenge ->
                warbler (fun ctx ->
                    "... todo - load from db ..." |> text
                )
            ) *)

            forward "/api/" apiRouter
        }

        let app = application {
            url ("http://0.0.0.0:8085/")
            use_router appRouter
            memory_cache
            use_static (currentApplication.PublicPath |> PublicPath.value)
            service_config (
                AppInsightTelemetry.configure currentApplication
                >> Logger.configure
            )
            use_gzip
        }

        run app

match [ ".env"; ".dist.env" ] |> CurrentApplication.fromEnvironment with
| Ok app -> WebApp.run app
| Error error -> failwithf "Application is not running due to error\n%A" error
