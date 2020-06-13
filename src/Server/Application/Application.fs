namespace MF.EDC

open MF.EDC.Profiler
open Microsoft.Azure.Cosmos.Table

type Debug =
    | Dev
    | Prod

[<RequireQualifiedAccess>]
module Debug =
    let parse = function
        | "dev" -> Dev
        | _ -> Prod

type CurrentApplication = {
    Instance: Instance
    TokenKey: JWTKey
    KeysForToken: JWTKey list
    //Logger: ApplicationLogger
    Debug: Debug
    ProfilerToken: Shared.Profiler.Token option
    AppInsightKey: AppInsightKey option
    PublicPath: PublicPath
    Dependencies: Dependencies
}

and Dependencies = {
    MysqlDatabase: MySqlConnectionString
    AzureSqlDatabase: AzureSqlConnectionString
    StorageAccount: CloudStorageAccount
}

//
// Errors
//

[<RequireQualifiedAccess>]
type InstanceError =
    | VariableNotFoundError of string
    | InvalidFormatError of string

[<RequireQualifiedAccess>]
module InstanceError =
    let format = function
        | InstanceError.VariableNotFoundError name -> sprintf "Environment variable %A for application instance is not set." name
        | InstanceError.InvalidFormatError instanceString -> sprintf "Value %A for application instance is not in correct format or is empty." instanceString

[<RequireQualifiedAccess>]
type KeyVaultError =
    | VariableNotFoundError of string * secret: string
    | GetSecretError of exn * secret: string

[<RequireQualifiedAccess>]
module KeyVaultError =
    let format = function
        | KeyVaultError.VariableNotFoundError (name, secret) -> sprintf "Secret %A can not be get from kv due to: environment variable %A for Key Vault name is not set." secret name
        | KeyVaultError.GetSecretError (e, secret) -> sprintf "Getting a secret %A ended with error %A." secret e.Message

[<RequireQualifiedAccess>]
type ConnectionStringError =
    | ConnectionStringMissing of string
    | MissingPassword of string
    | GettingPassword of KeyVaultError

[<RequireQualifiedAccess>]
module ConnectionStringError =
    let format = function
        | ConnectionStringError.ConnectionStringMissing name -> sprintf "Environment variable %A for Database Connection string name is not set." name
        | ConnectionStringError.MissingPassword name -> sprintf "Environment variable %A for Database password is not set." name
        | ConnectionStringError.GettingPassword error -> error |> KeyVaultError.format // todo check ..

//
// Current Application module
//

[<RequireQualifiedAccess>]
module CurrentApplication =
    open ErrorHandling
    open ErrorHandling.Result.Operators

    [<RequireQualifiedAccess>]
    module private Environment =
        open System
        open System.IO

        let load files = result {
            do!
                match files |> List.tryFind File.Exists with
                | Some file -> file |> Environment.loadFromFile
                | _ -> Ok ()

            return Environment.getEnvs()
        }

        type GetEnvironmentValue<'Success, 'Error> = Map<string, string> -> (string -> 'Success) -> (string -> 'Error) -> string -> Result<'Success, 'Error>

        let private getEnvironmentValue environment success error name =
            environment
            |> Map.tryFind name
            |> Option.map success
            |> Result.ofOption (error name)

        let instance environment variableName =
            result {
                let! instanceString =
                    variableName
                    |> getEnvironmentValue environment id InstanceError.VariableNotFoundError

                return!
                    instanceString
                    |> Instance.parse
                    |> Result.ofOption (InstanceError.InvalidFormatError instanceString)
            }

        let debug environment variableName =
            environment
            |> Map.tryFind variableName
            |> Option.defaultValue "prod"
            |> Debug.parse

        [<RequireQualifiedAccess>]
        module KeyVault =
            open Azure.Identity
            open Azure.Security.KeyVault.Secrets

            let secret environment keyVaultEnvVar secret = asyncResult {
                let! keyVaultName =
                    keyVaultEnvVar
                    |> getEnvironmentValue environment id (fun envVar -> KeyVaultError.VariableNotFoundError (envVar, secret))
                    |> AsyncResult.ofResult

                let kvUri = sprintf "https://%s.vault.azure.net" keyVaultName
                let client = SecretClient(Uri(kvUri), DefaultAzureCredential(false))

                let! secretResult =
                    client.GetSecretAsync(secret)
                    |> AsyncResult.ofTaskCatch (fun e -> KeyVaultError.GetSecretError (e, secret))

                return secretResult.Value.Value
            }

        let storageAccount environment =
            let tryParse connectionString =
                match CloudStorageAccount.TryParse connectionString with
                | true, cloudStorage -> Some cloudStorage
                | _ -> None

            getEnvironmentValue environment tryParse (sprintf "Environment variable %A for Cloud storage account is not set.")
            >=> Result.ofOption "Cloud storage account could not be parsed. Connection string is either empty, or in wrong format."

        [<RequireQualifiedAccess>]
        module Database =
            let azureSqlConnectionString environment kvSecret (connectionStringVarName, adminPassEnvVar, adminPassSecret) = asyncResult {
                let! (ConnectionString connectionString) =
                    connectionStringVarName
                    |> getEnvironmentValue environment ConnectionString ConnectionStringError.ConnectionStringMissing
                    |> AsyncResult.ofResult

                let! (DatabasePassword adminPass) =
                    match adminPassEnvVar |> getEnvironmentValue environment DatabasePassword ConnectionStringError.MissingPassword with
                    | Ok predefinedPassword ->
                        AsyncResult.ofSuccess predefinedPassword
                    | _ ->
                        adminPassSecret
                        |> kvSecret
                        |> AsyncResult.map DatabasePassword
                        |> AsyncResult.mapError (KeyVaultError.format >> ConnectionStringError.MissingPassword)

                return connectionString.Replace("{your_password}", adminPass) |> ConnectionString |> AzureSqlConnectionString
            }

            let mysqlLocalConnectionString environment =
                getEnvironmentValue environment (ConnectionString >> MySqlConnectionString) ConnectionStringError.MissingPassword

        let profilerToken environment kvSecret (envVar, kvKey) =
            let token =
                envVar
                |> getEnvironmentValue environment Shared.Profiler.Token ignore

            match token with
            | Ok token -> Some token
            | _ ->
                kvKey
                |> kvSecret
                |> AsyncResult.map Shared.Profiler.Token
                |> AsyncResult.mapError KeyVaultError.format
                |> Async.RunSynchronously
                |> Result.toOption

        let publicPath environment =
            getEnvironmentValue environment PublicPath ignore
            >-> fun () -> Ok (PublicPath "../Client/public")

        let appInsightKey environment =
            getEnvironmentValue environment AppInsightKey ignore
            >> Result.toOption

    let fromEnvironment files =
        result {
            let! environment = files |> Environment.load
            let kvSecret = "KEY_VAULT_NAME" |> Environment.KeyVault.secret environment

            let! instance = "INSTANCE" |> Environment.instance environment <@> InstanceError.format
            let debug = "DEBUG" |> Environment.debug environment

            let tokenKey =
                match debug with
                | Dev -> JWTKey.forDevelopment
                | Prod -> JWTKey.generate()

            let! storageAccount = "STORAGE_CONNECTIONSTRING" |> Environment.storageAccount environment
            let! mysqlConnectionString = "MYSQLCONNSTR_localdb" |> Environment.Database.mysqlLocalConnectionString environment <@> ConnectionStringError.format

            let! azureSqlConnectionString =
                ("SQLAZURECONNSTR_mf-edc-db", "ADMIN_DB_PASS", "mf-edc-admin-pass")
                |> Environment.Database.azureSqlConnectionString environment kvSecret
                |> Async.RunSynchronously <@> ConnectionStringError.format

            let profilerToken = ("PROFILER_TOKEN", "profiler-token") |> Environment.profilerToken environment kvSecret
            let! publicPath = "public_path" |> Environment.publicPath environment
            let appInsightKey = "APPINSIGHTS_INSTRUMENTATIONKEY" |> Environment.appInsightKey environment

            return {
                Instance = instance
                TokenKey = tokenKey
                KeysForToken = [
                    tokenKey
                ]
                //Logger = logger
                Debug = debug
                ProfilerToken = profilerToken
                PublicPath = publicPath
                AppInsightKey = appInsightKey
                Dependencies = {
                    MysqlDatabase = mysqlConnectionString
                    AzureSqlDatabase = azureSqlConnectionString
                    StorageAccount = storageAccount
                }
            }
        }

    let instance { Instance = instance } = instance
