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

and Dependencies = NoDependenciesYet

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
                |> Option.defaultValue "dev"    // todo - use prod as default
                |> Debug.parse
        }

    let fromEnvironment files =
        result {
            let! environment = files |> Environment.load

            let! instance = "INSTANCE" |> Environment.instance environment <@> InstanceError.format
            let! debug = "DEBUG" |> Environment.debug environment

            let tokenKey =
                match debug with
                | Dev -> JWTKey.forDevelopment
                | Prod -> JWTKey.generate()

            return {
                Instance = instance
                TokenKey = tokenKey
                KeysForToken = [
                    tokenKey
                ]
                //Logger = logger
                Debug = debug
                Dependencies = NoDependenciesYet
            }
        }

    let instance { Instance = instance } = instance
