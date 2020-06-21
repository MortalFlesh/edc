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

let inputField formName onSubmit isEnabled (input: Input) onChange title errors value =
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
                    Input.Id (sprintf "%s-%s" formName (title.Replace(" ", "")))

                    if errors |> List.isEmpty |> not then Input.Color IsDanger
                ]

                errors
                |> List.map (fun (ErrorMessage error) -> Help.help [ Help.Color IsDanger ] [ str error ])
                |> div []
            ]
        ]
    ]

let submit onSubmit status title  =
    Button.button [
        Button.IsLoading (status = InProgress)
        Button.Color IsPrimary
        Button.OnClick (fun _ -> onSubmit())
    ] [ str title ]

let table headers row items =
    Table.table [
        Table.IsBordered
        Table.IsFullWidth
        Table.IsStriped
        Table.IsHoverable
    ] [
        thead [] [
            tr [] [
                yield! headers |> List.map (fun header -> th [] [ str header ])
            ]
        ]

        tbody [] [
            yield! items |> List.map (row >> fun cols ->
                tr [] [
                    yield! cols |> List.map (fun col -> td [] [ col ])
                ]
            )
        ]
    ]
