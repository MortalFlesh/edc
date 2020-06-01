namespace MF.EDC

[<RequireQualifiedAccess>]
module Dto =
    open Shared
    open ErrorHandling

    [<RequireQualifiedAccess>]
    module Serialize =
        let user: User -> Dto.Login.User =
            fun { Username = username; Token = token } ->
                {
                    Username = username
                    Token = token
                }

    [<RequireQualifiedAccess>]
    module Deserialize =
        open ErrorHandling.Result.Operators

        let credentials: Username * Password -> Result<Credentials, CredentialsError> = function
            | Username "", Password "" -> Error EmptyCredentials
            | Username "", _ -> Error EmptyUsername
            | _, Password "" -> Error EmptyPassword
            | username, password -> Ok { Username = username; Password = password }
