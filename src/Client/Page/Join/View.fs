[<RequireQualifiedAccess>]
module PageJoin

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared
open PageJoinModule

let page (model: PageJoinModel) (dispatch: DispatchPageJoinAction) =
    let isJoining = model.JoinStatus = InProgress
    let onSubmit = if isJoining then ignore else (fun _ -> dispatch Join)

    let profileModel = model.NewUserProfile

    let fieldErrors field =
        model.Errors
        |> Map.tryFind field
        |> Option.defaultValue []

    let inputField input field =
        Component.inputField
            "Join"
            onSubmit
            (not isJoining)
            input
            (PageJoinAction.changeField field >> dispatch)
            (field |> Field.title)
            (field |> fieldErrors)

    fragment [] [
        Columns.columns [] [
            Column.column [ Column.Width (Screen.All, Column.Is12) ] [
                Columns.columns [ Columns.IsMultiline ] [
                    Column.column [ Column.Width (Screen.All, Column.Is6) ] [
                        // todo - show help -> you can use either of username or email for login

                        profileModel.Email
                        |> Option.defaultValue ""
                        |> inputField Input.email Email
                        // todo - show help -> "Email is for communication only, it will be hidden for others."

                        profileModel.Username
                        |> Option.defaultValue ""
                        |> inputField Input.text Username
                        // todo - show help -> "Username will be visible for others."
                    ]

                    Column.column [ Column.Width (Screen.All, Column.Is6) ] [
                        profileModel.Password
                        |> Option.defaultValue ""
                        |> inputField Input.password Password

                        profileModel.PasswordCheck
                        |> Option.defaultValue ""
                        |> inputField Input.password PasswordCheck
                    ]

                    Column.column [ Column.Width (Screen.All, Column.Is1) ] [
                        "Join" |> Component.submit onSubmit model.JoinStatus
                    ]

                    match model.Errors |> Map.tryFind Form with
                    | Some errors ->
                        errors
                        |> List.map (fun (ErrorMessage error) ->
                            Help.help [ Help.Color IsDanger ] [ str error ]
                        )
                        |> Column.column [ Column.Width (Screen.All, Column.Is12) ]
                    | _ -> ()
                ]
            ]
        ]
    ]
