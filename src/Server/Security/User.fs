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

[<RequireQualifiedAccess>]
type EmailError =
    | Empty
    | WrongFormat

[<RequireQualifiedAccess>]
module EmailError =
    let format = function
        | EmailError.Empty -> "You have to pass in an e-mail address."
        | EmailError.WrongFormat -> "Given e-mail address is not in correct format."

type UserError<'LoginError> =
    | LoginError of 'LoginError

[<RequireQualifiedAccess>]
module UserError =
    let format (formatLoginError: 'LoginError -> string) = function
        | LoginError error -> error |> formatLoginError

//
// Types
//

[<RequireQualifiedAccess>]
module Email =
    open System
    open System.Net.Mail

    let create email =
        try MailAddress(email).Address |> Email |> Ok
        with
        | :? FormatException -> Error EmailError.WrongFormat
        | :? ArgumentNullException
        | :? ArgumentException -> Error EmailError.Empty
        | _ -> Error EmailError.WrongFormat

[<AutoOpen>]
module PasswordModule =
    type private PasswordValue =
        | UserInput of string
        | Encrypted of string

    type Password = private Password of PasswordValue

    let (|EmptyPassword|_|) = function
        | Password (UserInput null) | Password (UserInput "")
        | Password (Encrypted null) | Password (Encrypted "") -> Some EmptyPassword
        | _ -> None

    type CurrentPassword = Current of string
    type TypedInPassword = TypedIn of Password

    [<RequireQualifiedAccess>]
    module Password =
        open BCrypt.Net

        let empty = Password (UserInput "")
        let ofUserInput (Shared.Password userPassword) = Password (UserInput userPassword)

        let encrypt = function
            | Password (UserInput value) ->
                value
                |> BCrypt.HashPassword
                |> Encrypted
                |> Password
            | password -> password

        let inputValue = function
            | Password (UserInput password) -> Some password
            | _ -> None

        let encryptedValue = function
            | Password (Encrypted password) -> Some password
            | _ -> None

        let verify (Current currentPassword, TypedIn typedPassword) =
            if BCrypt.Verify(typedPassword |> inputValue |> Option.defaultValue "", currentPassword)
                then Some ()
                else None

[<RequireQualifiedAccess>]
type UsernameOrEmail =
    | Username of Username
    | Email of Email
    | UsernameOrEmail of string    // todo - this should be in login form, ...

[<RequireQualifiedAccess>]
module UsernameOrEmail =
    let value = function
        | UsernameOrEmail.Username username -> username |> Username.value
        | UsernameOrEmail.Email email -> email |> Email.value
        | UsernameOrEmail.UsernameOrEmail value -> value

    let valueWithType = function
        | UsernameOrEmail.Username username -> username |> Username.value |> sprintf "username %s"
        | UsernameOrEmail.Email email -> email |> Email.value |> sprintf "e-mail %s"
        | UsernameOrEmail.UsernameOrEmail value -> value

type Credentials = {
    Username: Username
    Password: Password
}

type User = {
    Id: Id
    Username: Username
    Token: JWTToken
}

[<RequireQualifiedAccess>]
module User =
    open ErrorHandling.Result.Operators

    let login authorize credentials =
        credentials
        |> authorize <@> LoginError

type UserType =
    | Registered

[<RequireQualifiedAccess>]
module UserType =
    let value = function
        | Registered -> "Registered"

type UserProfile = {
    Id: Id
    Username: Username
    Email: Email
    Slug: Slug
}
