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

        let inline (>??>) authorize action =
            Authorize.authorizeActionWithUser
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

            Join = fun (email, username, password) -> asyncResult {
                let! email =
                    email
                    |> Dto.Deserialize.email
                    |> AsyncResult.ofResult <@> (EmailError.format >> ErrorMessage)

                let! credentials =
                    (username, password)
                    |> Dto.Deserialize.credentials
                    |> AsyncResult.ofResult <@> (CredentialsError.format >> ErrorMessage)

                let usernameOrEmail = [
                    email |> UsernameOrEmail.Email
                    credentials.Username |> UsernameOrEmail.Username
                ]

                let! slug =
                    credentials.Username
                    |> Username.value
                    |> Slug.create
                    |> Result.ofOption (ErrorMessage "Try to use a different username.")
                    |> AsyncResult.ofResult

                let! isUserExistsResults =
                    usernameOrEmail
                    |> List.map (UsersQuery.isUserExists logError currentApplication.Dependencies.StorageAccount slug)
                    |> Async.Parallel
                    |> AsyncResult.ofAsyncCatch (fun e ->
                        e.Message
                        |> sprintf "Join.IsUserExists error: %A"
                        |> logError

                        ErrorMessage "Please try it again, we have some issues."
                    )

                let! isUserExists =
                    isUserExistsResults
                    |> Seq.toList
                    |> Result.sequence
                    |> AsyncResult.ofResult <@> (UsersError.format >> ErrorMessage)

                do!
                    match isUserExists |> List.zip usernameOrEmail with
                    | [ (_, true); (_, true) ] -> ErrorMessage "There is already a user with this username or e-mail." |> AsyncResult.ofError
                    | [ (usernameOrEmail, true); _ ]
                    | [ _; (usernameOrEmail, true) ] -> usernameOrEmail |> UsernameOrEmail.valueWithType |> sprintf "There is already a user with %s." |> ErrorMessage |> AsyncResult.ofError
                    | _ -> AsyncResult.ofSuccess ()

                let! userProfile =
                    (credentials.Username, credentials.Password, email, slug)
                    |> Command.UsersCommand.create currentApplication.Dependencies.StorageAccount <@> ErrorMessage

                let user = {
                    Id = userProfile.Id
                    Username = userProfile.Username
                    Token =
                        userProfile
                        |> JWTToken.create
                            currentApplication.Instance
                            currentApplication.TokenKey
                }

                return user |> Dto.Serialize.user
            }

            Login = fun credentials -> asyncResult {
                let! credentials =
                    credentials
                    |> Dto.Deserialize.credentials
                    |> AsyncResult.ofResult <@> (CredentialsError.format >> ErrorMessage)

                let usernameOrEmail =
                    // todo - allow email in loginCredentials as well.. (or just type it explicitly)
                    credentials.Username
                    |> Username.value
                    |> UsernameOrEmail.UsernameOrEmail

                let! userProfile =
                    UsersQuery.loadUser logError currentApplication.Dependencies.StorageAccount usernameOrEmail credentials.Password
                    <@> (UsersError.format >> ErrorMessage)

                let! userProfile =
                    userProfile
                    |> Result.ofOption (ErrorMessage "No user found by given credentials.")
                    |> AsyncResult.ofResult

                let user = {
                    Id = userProfile.Id
                    Username = userProfile.Username
                    Token =
                        userProfile
                        |> JWTToken.create
                            currentApplication.Instance
                            currentApplication.TokenKey
                }

                return user |> Dto.Serialize.user
            }

            LoadProfiler = fun token -> async {
                return
                    match currentApplication.Debug with
                    | Dev -> Profiler.init currentApplication.Instance (Environment.getEnvs()) (sprintf "%A" Dev) |> Some
                    | Prod when (Some token) = (Some currentApplication.ProfilerToken) -> Profiler.init currentApplication.Instance (Environment.getEnvs()) (sprintf "%A with token" Prod) |> Some
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

            ValidateTag = Authorize.withLogin >?> fun tag -> asyncResult {
                let! tag =
                    tag
                    |> Tag.parse
                    |> AsyncResult.ofOption (ErrorMessage "Invalid tag value.")

                return tag |> Dto.Serialize.tag
            }

            LoadItems = Authorize.withLogin >??> fun user () -> asyncResult {
                let! data =
                    ItemsQuery.load
                        logError
                        currentApplication.Dependencies.StorageAccount
                        user.Id
                    <@> (ItemsError.format >> ErrorMessage)

                return data |> List.map Dto.Serialize.itemEntity
            }

            CreateItem = Authorize.withLogin >??> fun user itemDto -> asyncResult {
                let! item =
                    itemDto
                    |> Dto.Deserialize.item
                    |> AsyncResult.ofResult <@> (sprintf "%A" >> ErrorMessage)
                    //|> AsyncResult.ofResult <@> (DeserializeItemError.format >> ErrorMessage)

                let! itemEntity =
                    item
                    |> Command.ItemsCommand.create currentApplication.Dependencies.StorageAccount user.Id <@> ErrorMessage

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
