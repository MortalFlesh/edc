module PageJoinModule

open Elmish

open Shared
open Shared.Dto.Login

type JoinError =
    | MissingEmail
    | WrongEmailFormat
    | MissingUsername
    | MissingPassword
    | MissingCredentials
    | JoinError of ErrorMessage

[<RequireQualifiedAccess>]
module JoinError =
    let format = function
        | MissingEmail -> "Please fill e-mail."
        | WrongEmailFormat -> "Given e-mail is not in correct format."
        | MissingUsername -> "Please fill Username."
        | MissingPassword -> "Please fill Password."
        | MissingCredentials -> "Please fill Credentials."
        | JoinError (ErrorMessage message) -> message

type Field =
    | Form
    | Email
    | Username
    | Password
    | PasswordCheck

type NewUserProfileData = {
    Email: string option
    Username: string option
    Password: string option
    PasswordCheck: string option
}

type ValidUserProfile = {
    Email: Email
    Username: Username
    Password: Password
}

type PageJoinModel = {
    NewUserProfile: NewUserProfileData
    Errors: Map<Field, ErrorMessage list>
    JoinStatus: AsyncStatus
}

[<RequireQualifiedAccess>]
module Field =
    let key = sprintf "%A"

    let fromKey = function
        | "Email" -> Email
        | "Username" -> Username
        | "Password" -> Password
        | "PasswordCheck" -> PasswordCheck
        | _ -> Form

    let title = function
        | Form -> ""
        | Email -> "Email"
        | Username -> "Username"
        | Password -> "Password"
        | PasswordCheck -> "Password Check"

    let private updateNewProfile (model: PageJoinModel) newProfile =
        { model with NewUserProfile = newProfile }

    let update (model: PageJoinModel) value = function
        | Form -> model
        | Email -> { model.NewUserProfile with Email = value |> String.parse } |> updateNewProfile model
        | Username -> { model.NewUserProfile with Username = value |> String.parse } |> updateNewProfile model
        | Password -> { model.NewUserProfile with Password = value |> String.parse } |> updateNewProfile model
        | PasswordCheck -> { model.NewUserProfile with PasswordCheck = value |> String.parse } |> updateNewProfile model

//
// Messages / Actions
//

type PageJoinAction =
    | InitPage
    | ChangeField of Field * string

    | Join
    | JoinSuccess of User
    | ShowError of JoinError

[<RequireQualifiedAccess>]
module PageJoinAction =
    let changeField field value = ChangeField (field, value)

type DispatchPageJoinAction = PageJoinAction -> unit

[<RequireQualifiedAccess>]
module Validate =
    open Fable.Validation.Core
    open Validations

    let private f = Field.key

    let newUserProfile (newUserProfile: NewUserProfileData): Validation<ValidUserProfile> = all <| fun t ->
        {
            Email = t.Test (f Email) (newUserProfile.Email |> Option.defaultValue "")
                |> t.Trim
                |> t.NotBlank "Please fill an e-mail."
                |> t.IsMail "Please fill a valid e-mail."
                |> t.Map Shared.Email
                |> t.End

            Username = t.Test (f Username) (newUserProfile.Username |> Option.defaultValue "")
                |> t.Trim
                |> t.NotBlank "Please fill a username."
                |> t.Map Shared.Username
                |> t.End

            Password = t.Test (f Password) (newUserProfile.Password |> Option.defaultValue "")
                |> t.Trim
                |> t.NotBlank "Please fill a password."
                |> t.MinLen 6 "Please fill a password, longer than {len} chars."
                |> t.IsValid (Some >> (=) newUserProfile.PasswordCheck) "Password is different than the password check."
                |> t.Map Shared.Password
                |> t.End
        }

[<RequireQualifiedAccess>]
module PageJoinModel =
    let empty = {
        NewUserProfile = {
            Email = None
            Username = None
            Password = None
            PasswordCheck = None
        }
        Errors = Map.empty
        JoinStatus = Inactive
    }

    let private clearError field (model: PageJoinModel) =
        { model with Errors = model.Errors.Remove(Form).Remove(field) }

    let private withErrors errors (model: PageJoinModel) =
        { model with
            Errors =
                errors
                |> Map.toList
                |> List.distinct
                |> List.fold (fun errors (field, fieldErrors) ->
                    errors.Add(Field.fromKey field, fieldErrors |> List.map ErrorMessage)
                ) model.Errors
        }

    let private withFieldError field error (model: PageJoinModel) =
        { model with
            Errors =
                model.Errors.Add(
                    field,
                    match model.Errors.TryFind field with
                    | Some errors -> errors @ [ error ] |> List.distinct
                    | _ -> [ error ]
                )
        }

    let private updateFieldAndClearError field value model =
        field
        |> Field.update model value
        |> clearError field, Cmd.none

    let update<'GlobalAction>
        (liftAction: PageJoinAction -> 'GlobalAction)
        (showSuccess: SuccessMessage -> 'GlobalAction)
        (showError: ErrorMessage -> 'GlobalAction)
        (model: PageJoinModel)
        : PageJoinAction -> PageJoinModel * Cmd<'GlobalAction> = function

        | InitPage -> empty, Cmd.none

        | ChangeField (field, value) -> model |> updateFieldAndClearError field value

        | Join ->
            match model.NewUserProfile |> Validate.newUserProfile with
            | Ok validProfile ->
                { model with JoinStatus = InProgress },
                (validProfile.Email, validProfile.Username, validProfile.Password)
                |> Api.join JoinSuccess (JoinError >> ShowError)
                |> Cmd.OfAsyncImmediate.result
                |> Cmd.map liftAction
            | Error errors -> model |> withErrors errors, Cmd.none

        | JoinSuccess _ ->
            { model with JoinStatus = Inactive }, Cmd.none

        | ShowError e ->
            { model with JoinStatus = Inactive }, Cmd.ofMsg (e |> JoinError.format |> ErrorMessage |> showError)
