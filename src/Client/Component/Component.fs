[<RequireQualifiedAccess>]
module Component

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared

let subTitleBody body =
    Heading.h5 [ Heading.IsSubtitle ] body

let subTitle text =
    subTitleBody [ str text ]

[<RequireQualifiedAccess>]
module Select =
    open Fable.Core.JsInterop

    let multi (rows: int) selectOptions options =
        Select.select [ Select.IsMultiple; Select.CustomClass "is-full-size" ] [
            select [
                ClassName "is-full-size"
                Multiple true
                Size (float rows)
                OnChange (fun e ->
                    let selectedValues =
                        e.target?selectedOptions
                        |> Seq.map (fun selectedOption -> selectedOption?value |> string)
                        |> Seq.toList

                    selectedValues |> selectOptions
                )
            ] options
        ]

[<RequireQualifiedAccess>]
module Icon =
    let private status = function
        | Some (color, icon, others) ->
            Icon.icon
              [ Icon.Size IsMedium
                Icon.Modifiers [ Modifier.TextColor color ]
                Icon.IsRight ]
              [ Fa.i (icon :: others) [] ]
        | None -> null

    let asyncStatus =
        (function
            | InProgress -> Some (IsBlack, Fa.Solid.Spinner, [ Fa.Pulse ])
            | Completed -> Some (IsSuccess, Fa.Solid.Check, [])
            | Inactive -> None
        ) >> status

    let medium icon =
        Icon.icon
          [ Icon.Size IsMedium ]
          [ Fa.i [ icon ] [] ]

[<RequireQualifiedAccess>]
module Button =
    let startAsync action status text =
        Button.button [
            if status = InProgress then
                Button.Disabled true
            else
                Button.OnClick (fun _ -> action ())
        ] [ str text ]

[<RequireQualifiedAccess>]
module Modal =
    let basic isActive closeDisplay content =
        let closeDisplay = fun _ -> closeDisplay()

        Modal.modal [ Modal.IsActive isActive ]
            [ Modal.background [ Props [ OnClick closeDisplay ] ] []
              Modal.content []
                [ Box.box' []
                    [ content ] ]
              Modal.close [ Modal.Close.Size IsLarge
                            Modal.Close.OnClick closeDisplay ] [] ]

    let card isActive closeDisplay title content footerContent =
        let closeDisplay = fun _ -> closeDisplay()

        Modal.modal [ Modal.IsActive isActive ] [
            Modal.background [ Props [ OnClick closeDisplay ] ] []
            Modal.Card.card [] [
                Modal.Card.head [] [
                    Modal.Card.title [] [ str title ]
                    Delete.delete [ Delete.OnClick closeDisplay ] []
                ]
                Modal.Card.body [] [ content ]
                Modal.Card.foot [] [ footerContent ]
            ]
        ]

type private Input = Input.Option list -> ReactElement

let inputField onSubmit isEnabled (input: Input) onChange title error value =
    Field.div [] [
        Field.div [ Field.HasAddons ] [
            Control.p [] [
                Button.button [ Button.IsStatic true; Button.Props [ TabIndex -1 ] ] [ str title ]
            ]
            Control.p [ Control.IsExpanded ] [
                input [
                    Input.Disabled (not isEnabled)
                    Input.OnChange (fun e -> e.Value |> onChange)
                    Input.Props [
                        OnKeyPress (fun e -> if e.key = "Enter" then onSubmit())
                    ]
                    Input.Placeholder title
                    Input.Value value

                    if error |> Option.isSome then Input.Color IsDanger
                ]

                match error with
                | Some (ErrorMessage error) -> Help.help [ Help.Color IsDanger ] [ str error ]
                | _ -> ()
            ]
        ]
    ]
