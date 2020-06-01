namespace MF.EDC

open Shared
open ErrorHandling

//
// Errors
//

type CredentialsError =
    | EmptyUsername
    | EmptyPassword
    | EmptyCredentials

[<RequireQualifiedAccess>]
module CredentialsError =
    let format = function
        | EmptyUsername -> "You have to pass in a credentials. Username is missing."
        | EmptyPassword -> "You have to pass in a credentials. Password is missing."
        | EmptyCredentials -> "You have to pass in a credentials. Both Username and Password are missing."

type UserError<'LoginError> =
    | LoginError of 'LoginError

[<RequireQualifiedAccess>]
module UserError =
    let format (formatLoginError: 'LoginError -> string) = function
        | LoginError error -> error |> formatLoginError

//
// Types
//

type Credentials = {
    Username: Username
    Password: Password
}

type User = {
    Username: Username
    Token: JWTToken
}

[<RequireQualifiedAccess>]
module User =
    open ErrorHandling.Result.Operators

    let login authorize credentials =
        credentials
        |> authorize <@> LoginError
