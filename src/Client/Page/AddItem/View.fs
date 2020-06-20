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
open Shared.Dto.Common
open Shared.Dto.Items

let page (model: PageAddItemModel) (dispatch: DispatchPageAddItemAction) =
    let isSubmitting = model.SavingStatus = InProgress
    let itemModel = model.NewItem

    let fieldErrors field =
        model.Errors
        |> Map.tryFind field
        |> Option.defaultValue []

    let onSubmit = if isSubmitting then ignore else (fun _ -> dispatch PageAddItemAction.SaveItem)
    let inputField input field =
        Component.inputField
            "AddItem"
            onSubmit
            (not isSubmitting)
            input
            (PageAddItemAction.changeField field >> dispatch)
            (field |> Field.title)
            (field |> fieldErrors)

    Columns.columns [] [
        Column.column [ Column.Width (Screen.All, Column.Is12) ] [
            Component.subTitle "Add Item"
        ]
        Column.column [ Column.Width (Screen.All, Column.Is12) ] [
            Columns.columns [ Columns.IsMultiline ] [
                Column.column [ Column.Width (Screen.All, Column.Is6) ] [
                    // todo show global Form errors (Field.Form)

                    // todo - add selector for Type and SubType

                    Component.subTitle "Common"

                    itemModel.Name
                    |> inputField Input.text Name

                    itemModel.Note
                    // todo - make i text area or wysiwyg
                    |> Option.defaultValue ""
                    |> inputField Input.text Note

                    model.TagsModel
                    |> Tag.inputField
                        "AddItem"
                        onSubmit
                        (not isSubmitting)
                        "Tags"
                        (PageAddItemAction.TagsInputAction >> dispatch)

                    itemModel.Color
                    |> Option.defaultValue ""
                    |> inputField Input.text Field.Color

                    (* itemModel.Links
                    |> Option.defaultValue ""
                    |> inputField Input.text Links *)

                    itemModel.OwnershipStatus   // todo - select box
                    |> Option.defaultValue ""
                    |> inputField Input.text OwnershipStatus
                ]

                Column.column [ Column.Width (Screen.All, Column.Is6) ] [
                    Component.subTitle "Product"

                    model.NewProduct |> Product.form onSubmit isSubmitting (PageAddItemAction.ProductAction >> dispatch)
                ]
            ]

            Columns.columns [ Columns.IsMultiline ] [
                Column.column [ Column.Width (Screen.All, Column.Is6) ] [

                    ("Save", "Saving")
                    |> Component.submit onSubmit model.SavingStatus

                ]
            ]
        ]
    ]
