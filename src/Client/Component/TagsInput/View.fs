[<RequireQualifiedAccess>]
module Tag

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared
open Shared.Dto.Common

open TagsInputModule

let inputField formName onSubmit isEnabled title (dispatch: DispatchTagsInputAction) (model: TagsInputModel) =
    //  (on focus -> show hint "Use Tab or separate by , to create multiple Tags")

    Field.div [] [
        Field.div [ Field.HasAddons ] [
            Control.p [] [
                Button.button [ Button.CustomClass "no-border-right"; Button.IsStatic true; Button.Props [ TabIndex -1 ] ] [ str title ]
            ]
            div [ ClassName "tag-input-list" ] [
                yield!
                    model.Tags
                    |> List.map (function
                        | Ok tag -> tag |> Tag.value |> Tag.deleteable (TagsInputAction.DeleteTag >> dispatch) IsSuccess
                        | Error (WrongTag tag) -> tag |> Tag.deleteable (TagsInputAction.DeleteTag >> dispatch) IsDanger
                    )
            ]
            Control.p [ Control.IsExpanded ] [
                Input.text [
                    Input.CustomClass "no-border-left"
                    Input.Disabled (not isEnabled)
                    Input.OnChange (fun e -> e.Value |> TagsInputAction.ChangeValue |> dispatch)
                    Input.Props [
                        OnKeyPress (fun e -> if e.key = "Enter" then onSubmit())
                        // todo - On tab - if the value is not empty -> dispatch ChangeValue with (value + " ")
                        //  - and do the same for Enter
                    ]
                    Input.Placeholder title
                    Input.Id (sprintf "%s-%s" formName title)
                    Input.Value model.NewTag
                ]

                model.Validating |> Component.Icon.asyncStatus

                match model.Error with
                | Some (ErrorMessage error) -> div [] [Help.help [ Help.Color IsDanger ] [ str error ]]
                | _ -> ()
            ]
        ]
    ]
