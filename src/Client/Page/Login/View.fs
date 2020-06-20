[<RequireQualifiedAccess>]
module PageLogin

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared
open PageLoginModule

let page (model: PageLoginModel) (dispatch: DispatchPageLoginAction) =
    let isLogging = model.LoginStatus = InProgress
    let onSubmit = if isLogging then ignore else (fun _ -> dispatch Login)
    let inputField = Component.inputField "Login" onSubmit (not isLogging)

    fragment [] [
        Columns.columns [] [
            Column.column [ Column.Width (Screen.All, Column.Is12) ] [
                Columns.columns [ Columns.IsMultiline ] [
                    Column.column [ Column.Width (Screen.All, Column.Is5) ] [
                        model.Username
                        |> Username.value
                        |> inputField Input.text
                            (Username >> PageLoginAction.ChangeUsername >> dispatch)
                            "Username"
                            (model.UsernameError |> Option.toList)
                    ]

                    Column.column [ Column.Width (Screen.All, Column.Is5) ] [
                        model.Password
                        |> Password.value
                        |> inputField Input.password
                            (Password >> PageLoginAction.ChangePassword >> dispatch)
                            "Password"
                            (model.PasswordError |> Option.toList)
                    ]

                    Column.column [ Column.Width (Screen.All, Column.Is1); Column.Offset (Screen.All, Column.Is1) ] [
                        Button.a [
                            Button.Disabled isLogging
                            Button.Color IsSuccess
                            Button.OnClick (fun _ -> onSubmit())
                        ] [
                            if isLogging
                                then
                                    model.LoginStatus |> Component.Icon.asyncStatus
                                    span [] [ str " " ]
                                    str "Logging ..."
                                else
                                    str "Login"
                        ]
                    ]

                    match model.LoginError with
                    | Some (ErrorMessage loginError) ->
                        Column.column [ Column.Width (Screen.All, Column.Is12) ] [
                            Help.help [ Help.Color IsDanger ] [ str loginError ]
                        ]
                    | _ -> ()
                ]
            ]
        ]
    ]
