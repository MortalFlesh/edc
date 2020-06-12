namespace MF.EDC

open MF.EDC.Profiler

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
    Dependencies: Dependencies
}

and Dependencies = {
    MysqlDatabase: ConnectionString
    AzureSqlDatabase: ConnectionString
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
    | VariableNotFoundError of string
    | GetSecretError of exn

[<RequireQualifiedAccess>]
module KeyVaultError =
    let format = function
        | KeyVaultError.VariableNotFoundError name -> sprintf "Environment variable %A for Key Vault name is not set." name
        | KeyVaultError.GetSecretError e -> sprintf "Getting a secret ended with error %A." e.Message

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

        let debug environment variableName = result {
            return
                environment
                |> Map.tryFind variableName
                |> Option.defaultValue "prod"
                |> Debug.parse
        }

        [<RequireQualifiedAccess>]
        module KeyVault =
            open Azure.Identity
            open Azure.Security.KeyVault.Secrets

            let secret environment secret = asyncResult {
                let! keyVaultName =
                    "KEY_VAULT_NAME"
                    |> getEnvironmentValue environment id KeyVaultError.VariableNotFoundError
                    |> AsyncResult.ofResult

                let kvUri = sprintf "https://%s.vault.azure.net" keyVaultName
                let client = SecretClient(Uri(kvUri), DefaultAzureCredential(false))

                let! secretResult =
                    client.GetSecretAsync(secret)
                    |> Async.AwaitTask
                    |> Async.Catch
                    |> Async.map (Result.ofChoice >@> KeyVaultError.GetSecretError)

                return secretResult.Value.Value
            }

        [<RequireQualifiedAccess>]
        module Database =
            let azureSqlConnectionString environment = asyncResult {
                let! (ConnectionString connectionString) =
                    "SQLAZURECONNSTR_mf-edc-db"
                    |> getEnvironmentValue environment ConnectionString ConnectionStringError.ConnectionStringMissing
                    |> AsyncResult.ofResult

                let! (DatabasePassword adminPass) =
                    match "ADMIN_DB_PASS" |> getEnvironmentValue environment DatabasePassword ConnectionStringError.MissingPassword with
                    | Ok predefinedPassword ->
                        AsyncResult.ofSuccess predefinedPassword
                    | _ ->
                        "mf-edc-admin-pass"
                        |> KeyVault.secret environment
                        |> AsyncResult.map DatabasePassword
                        |> AsyncResult.mapError (KeyVaultError.format >> ConnectionStringError.MissingPassword)

                return connectionString.Replace("{your_password}", adminPass) |> ConnectionString
            }

            let mysqlLocalConnectionString environment =
                "MYSQLCONNSTR_localdb"
                |> getEnvironmentValue environment ConnectionString ConnectionStringError.MissingPassword

    let fromEnvironment files =
        result {
            let! environment = files |> Environment.load

            let! instance = "INSTANCE" |> Environment.instance environment <@> InstanceError.format
            let! debug = "DEBUG" |> Environment.debug environment

            let tokenKey =
                match debug with
                | Dev -> JWTKey.forDevelopment
                | Prod -> JWTKey.generate()

            let! mysqlConnectionString =
                Environment.Database.mysqlLocalConnectionString environment <@> ConnectionStringError.format

            let! azureSqlConnectionString =
                Environment.Database.azureSqlConnectionString environment
                |> Async.RunSynchronously <@> ConnectionStringError.format

            return {
                Instance = instance
                TokenKey = tokenKey
                KeysForToken = [
                    tokenKey
                ]
                //Logger = logger
                Debug = debug
                Dependencies = {
                    MysqlDatabase = mysqlConnectionString
                    AzureSqlDatabase = azureSqlConnectionString
                }
            }
        }

    let instance { Instance = instance } = instance
