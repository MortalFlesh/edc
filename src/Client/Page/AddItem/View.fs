[<RequireQualifiedAccess>]
module PageAddItem

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open PageAddItemModule
open Shared.Dto.Items

let page (model: PageAddItemModel) (dispatch: DispatchPageAddItemAction) =
    let isSubmitting = model.SavingStatus = InProgress

    let field = Field.value
    let errors =
        model.Errors
        |> Map.toList
        |> List.map (fun (field, error) -> field |> Field.value, error)
        |> Map.ofList

    let onSubmit = if isSubmitting then ignore else (fun _ -> dispatch PageAddItemAction.SaveItem)
    let inputField input title onChange = Component.inputField onSubmit (not isSubmitting) input onChange title errors

    div [] [
        Component.subTitle "Add Item"

        Columns.columns [] [
            Column.column [ Column.Width (Screen.All, Column.Is12) ] [
                Columns.columns [ Columns.IsMultiline ] [
                    Column.column [ Column.Width (Screen.All, Column.Is5) ] [

                        model.CommonInfo.Name
                        |> inputField Input.text (field Name) (PageAddItemAction.ChangeName >> dispatch)

                        model.CommonInfo.Note
                        |> Option.defaultValue ""
                        |> inputField Input.text (field Note) (PageAddItemAction.ChangeNote >> dispatch)

                        // links (text area |> parse "\n" |> List.choose Link.parse)

                        // tags (input
                        //  (on "tab" -> create tag ))
                        //  (on focus -> show hint "Use Tab or separate by , to create multiple Tags")

                    ]
                ]

                Columns.columns [ Columns.IsMultiline ] [
                    Column.column [ Column.Width (Screen.All, Column.Is5) ] [

                        ("Save", "Saving")
                        |> Component.submit onSubmit model.SavingStatus

                    ]
                ]
            ]
        ]
    ]
