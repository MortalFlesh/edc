module PageLoginModule

open Elmish

open Shared
open Shared.Dto.Login

type LoginError =
    | MissingUsername
    | MissingPassword
    | MissingCredentials
    | LoginError of ErrorMessage

[<RequireQualifiedAccess>]
module LoginError =
    let format = function
        | MissingUsername -> "Please fill Username."
        | MissingPassword -> "Please fill Password."
        | MissingCredentials -> "Please fill Credentials."
        | LoginError (ErrorMessage message) -> message

type PageLoginModel = {
    Username: Username
    Password: Password
    UsernameError: ErrorMessage option
    PasswordError: ErrorMessage option
    LoginError: ErrorMessage option

    User: User option
    LoginStatus: AsyncStatus
}

//
// Messages / Actions
//

type PageLoginAction =
    | InitPage

    | ChangeUsername of Username
    | ChangePassword of Password
    | Login
    | LoginSuccess of User
    | ShowError of LoginError

type DispatchPageLoginAction = PageLoginAction -> unit

[<RequireQualifiedAccess>]
module PageLoginModel =
    let empty = {
        Username = Username.empty
        Password = Password.empty
        UsernameError = None
        PasswordError = None
        LoginError = None

        User = None
        LoginStatus = Inactive
    }

    let private withoutErrors model: PageLoginModel =
        { model with UsernameError = None; PasswordError = None; LoginError = None }

    let private addError error model: PageLoginModel =
        let message = error |> LoginError.format |> ErrorMessage

        match error with
        | MissingUsername -> { model with UsernameError = Some message }
        | MissingPassword -> { model with PasswordError = Some message }
        | MissingCredentials
        | LoginError _ -> { model with LoginError = Some message }

    let private withError error = withoutErrors >> addError error

    let update<'GlobalAction>
        (liftAction: PageLoginAction -> 'GlobalAction)
        (showSuccess: SuccessMessage -> 'GlobalAction)
        (showError: ErrorMessage -> 'GlobalAction)
        (model: PageLoginModel) = function

        | InitPage -> empty, Cmd.none

        | ChangeUsername username ->
            { model with Username = username } |> withoutErrors, Cmd.none

        | ChangePassword password ->
            { model with Password = password } |> withoutErrors, Cmd.none

        | ShowError (LoginError error) ->
            { model with LoginStatus = Inactive }, LoginError error |> LoginError.format |> ErrorMessage |> showError |> Cmd.ofMsg

        | ShowError MissingCredentials ->
            model
            |> addError MissingCredentials
            |> addError MissingUsername
            |> addError MissingPassword
            , Cmd.none |> Cmd.map liftAction

        | ShowError error ->
            model |> withError error, Cmd.none |> Cmd.map liftAction

        | Login ->
            let showError = ShowError >> liftAction >> Cmd.ofMsg

            match model with
            | { UsernameError = Some _ }
            | { PasswordError = Some _ } -> model, Cmd.none

            | { Username = Username ""; Password = Password "" } -> model, MissingCredentials |> showError
            | { Username = Username "" } -> model, MissingUsername |> showError
            | { Password = Password "" } -> model, MissingPassword |> showError

            | { Username = username; Password = password } ->
                { model with LoginStatus = InProgress },
                (username, password)
                |> Api.login LoginSuccess (LoginError >> ShowError)
                |> Cmd.OfAsyncImmediate.result
                |> Cmd.map liftAction

        | LoginSuccess user ->
            { model with LoginStatus = Inactive; User = Some user }, Cmd.none
