module PageAddItemModule

open Elmish

open Shared
open Shared.Dto.Common
open Shared.Dto.Items

open TagsInputModule
open ProductModule

type Field =
    | Form
    | Name
    | Note
    | Color
    | Links
    | OwnershipStatus

type NewItemUserData = {
    Name: string
    Note: string option
    Color: string option
    Links: string list
    Weight: int option
    Dimensions: Dimensions option   // todo
    OwnershipStatus: string option
}

type ValidNewItem = {
    Name: string
    Note: string option
    Color: string option
    Links: string list
    Weight: int option
    Dimensions: Dimensions option   // todo
    OwnershipStatus: OwnershipStatus
}

type PageAddItemModel = {
    NewItem: NewItemUserData
    TagsModel: TagsInputModel
    NewProduct: ProductModel
    Errors: Map<Field, ErrorMessage list>
    SavingStatus: AsyncStatus
}

[<RequireQualifiedAccess>]
module Field =
    let key = sprintf "%A"

    let fromKey = function
        | "Name" -> Name
        | "Note" -> Note
        | "Color" -> Color
        | "Links" -> Links
        | "OwnershipStatus" -> OwnershipStatus
        | _ -> Form

    let title = function
        | Form -> ""
        | Name -> "Name"
        | Note -> "Note"
        | Color -> "Color"
        | Links -> "Links"
        | OwnershipStatus -> "Ownership"

    let private updateNewItem (model: PageAddItemModel) newItem =
        { model with NewItem = newItem }

    let update (model: PageAddItemModel) value = function
        | Form -> model
        | Name -> { model.NewItem with Name = value } |> updateNewItem model
        | Note -> { model.NewItem with Note = value |> String.parse } |> updateNewItem model
        | Color -> model // { model with Color = value }
        | Links -> model // { model with Links = value }
        | OwnershipStatus -> { model.NewItem with OwnershipStatus = value |> String.parse } |> updateNewItem model

//
// Messages / Actions
//

[<RequireQualifiedAccess>]
type PageAddItemAction =
    | InitPage
    | ChangeField of Field * string

    | SaveItem
    | ItemSaved of Item
    | ItemSavedWithError of ErrorMessage

    | TagsInputAction of TagsInputAction
    | ProductAction of ProductAction

[<RequireQualifiedAccess>]
module PageAddItemAction =
    let changeField field value = PageAddItemAction.ChangeField (field, value)

type DispatchPageAddItemAction = PageAddItemAction -> unit

[<RequireQualifiedAccess>]
module Validate =
    open Fable.Validation.Core
    open Validations

    let private f = Field.key

    let newItem (newItem: NewItemUserData): Validation<ValidNewItem> = all <| fun t ->
        {
            Name = t.Test (f Name) newItem.Name
                |> t.Trim
                |> t.NotBlank "Please fill Item name."
                |> t.MinLen 3 "Please make it more descriptive, minimal length is {len}."
                |> t.MaxLen 200 "Please make it shorter, max allowed length is {len}."
                |> t.End

            Note = t.TestOnlySome (f Note) newItem.Note [
                t.Trim
                t.MinLen 5 "Please make it more descriptive, minimal length is {len}."
                t.MaxLen 500 "Please make it shorter, max allowed length is {len}."
            ]

            Color = None

            Links = []

            Weight = newItem.Weight

            Dimensions = None

            OwnershipStatus = t.Test (f OwnershipStatus) (newItem.OwnershipStatus |> Option.bind OwnershipStatus.parse)
                |> t.IsSome "Please select an Ownership."
                |> t.End
        }

[<RequireQualifiedAccess>]
module PageAddItemModel =
    let empty = {
        NewItem = {
            Name = ""
            Note = None
            Color = None
            Links = []
            Weight = None
            Dimensions = None
            OwnershipStatus = None
        }
        TagsModel = TagsInputModel.empty
        NewProduct = ProductModel.empty
        Errors = Map.empty
        SavingStatus = Inactive
    }

    let private clearError field (model: PageAddItemModel) =
        { model with Errors = model.Errors.Remove(Form).Remove(field) }

    let private withErrors errors (model: PageAddItemModel) =
        { model with
            Errors =
                errors
                |> Map.toList
                |> List.distinct
                |> List.fold (fun errors (field, fieldErrors) ->
                    errors.Add(Field.fromKey field, fieldErrors |> List.map ErrorMessage)
                ) model.Errors
        }

    let private withFieldError field error (model: PageAddItemModel) =
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
        (liftAction: PageAddItemAction -> 'GlobalAction)
        (showSuccess: SuccessMessage -> 'GlobalAction)
        (showError: ErrorMessage -> 'GlobalAction)
        (authError: ErrorMessage -> 'GlobalAction)
        (model: PageAddItemModel)
        : PageAddItemAction -> PageAddItemModel * Cmd<'GlobalAction> = function

        | PageAddItemAction.InitPage ->
            empty, Cmd.none

        | PageAddItemAction.ChangeField (field, value) -> model |> updateFieldAndClearError field value

        | PageAddItemAction.SaveItem ->
            let p = model.NewProduct.NewProduct |> Validate.newProduct
            let i = model.NewItem |> Validate.newItem

            match i, p with
            | Ok validItem, Ok validProduct ->
                let item = Item.Tool <| Tool.Knife {
                    Common = {
                        Name = validItem.Name
                        Note = validItem.Note
                        Color = validItem.Color |> Option.map Dto.Common.Color
                        Tags = model.TagsModel.Tags |> List.choose Result.toOption
                        Links = validItem.Links |> List.map Link
                        Price = None    // todo<later> - from Price sub component
                        Weight = validItem.Weight |> Option.map Weight.ofGrams
                        Dimensions = validItem.Dimensions
                        Product = Some { // todo<later> - this could be none, where there will be checkbox or other identificator, whether prodcut should be there
                            Id = validProduct.Id |> Option.map Id.value |> Option.defaultValue ""
                            Name = validProduct.Name
                            Manufacturer = validProduct.Manufacturer
                            Price = {   // todo
                                Amount = 0.0
                                Currency = "Czk"
                            }
                            Ean = validProduct.Ean
                            Links = validProduct.Links
                        }

                        OwnershipStatus = validItem.OwnershipStatus
                        Gallery = None  // todo<later> - from Gallery sub component
                    }
                }

                let create =
                    Api.createItem
                        (PageAddItemAction.ItemSaved >> liftAction)
                        (PageAddItemAction.ItemSavedWithError >> liftAction)
                        authError

                { model with SavingStatus = InProgress }, Cmd.OfAsyncImmediate.result (create item)

            | Error itemErrors, Error productErrors ->
                { model with
                    NewProduct = model.NewProduct |> ProductModel.withErrors productErrors
                } |> withErrors itemErrors, Cmd.none

            | Error errors, _ -> model |> withErrors errors, Cmd.none
            | _, Error errors -> { model with NewProduct = model.NewProduct |> ProductModel.withErrors errors }, Cmd.none

        | PageAddItemAction.ItemSaved item ->
            let data =
                item
                |> FlatItem.FlatItem.ofItem
                |> FlatItem.FlatItem.data

            { model with SavingStatus = Completed }, Cmd.ofMsg (showSuccess (SuccessMessage (sprintf "Item %A was successfully created." data.Common.Name )))

        | PageAddItemAction.ItemSavedWithError e ->
            { model with SavingStatus = Inactive }, Cmd.ofMsg (showError e)

        //
        // Smart components
        //
        | PageAddItemAction.TagsInputAction action ->
            let (tagsModel, action) = action |> TagsInputModel.update PageAddItemAction.TagsInputAction model.TagsModel
            { model with TagsModel = tagsModel }, action |> Cmd.map liftAction

        | PageAddItemAction.ProductAction action ->
            let (productModel, action) = action |> ProductModel.update PageAddItemAction.ProductAction model.NewProduct
            { model with NewProduct = productModel }, action |> Cmd.map liftAction
