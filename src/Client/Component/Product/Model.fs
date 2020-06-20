module ProductModule

open Elmish

open Shared
open Shared.Dto.Common

type Field =
    | Form
    | Name
    | Manufacturer
    | Price
    | Ean
    | Links

type NewProductData = {
    // todo<later> - this will be handled by selecting or creating a new product
    Id: Id option
    Name: string option
    Manufacturer: string option
    Price: string option
    Ean: Ean option
    Links: Link list
}

type ValidProduct = {
    // todo<later> - this will be handled by selecting or creating a new product
    Id: Id option
    Name: string
    Manufacturer: string
    Price: string option
    Ean: Ean option
    Links: Link list
}

type ProductModel = {
    NewProduct: NewProductData
    Errors: Map<Field, ErrorMessage list>
}

[<RequireQualifiedAccess>]
module Field =
    let key: Field -> string = sprintf "%A"

    let fromKey = function
        | "Name" -> Name
        | "Manufacturer" -> Manufacturer
        | "Price" -> Price
        | "Ean" -> Ean
        | "Links" -> Links
        | _ -> Form

    let title = function
        | Form -> ""
        | Name -> "Name"
        | Manufacturer -> "Manufacturer"
        | Price -> "Price"
        | Ean -> "Ean"
        | Links -> "Links"

    let private updateNewProduct model newProduct =
        { model with NewProduct = newProduct }

    let update (model: ProductModel) value = function
        | Form -> model
        | Name -> { model.NewProduct with Name = value |> String.parse } |> updateNewProduct model
        | Manufacturer -> { model.NewProduct with Manufacturer = value |> String.parse } |> updateNewProduct model
        | Price -> model // todo
        | Ean -> { model.NewProduct with Ean = value |> String.parse |> Option.map Dto.Common.Ean } |> updateNewProduct model
        | Links -> model // todo

[<RequireQualifiedAccess>]
type ProductAction =
    | ChangeField of Field * string

[<RequireQualifiedAccess>]
module ProductAction =
    let changeField field value = ProductAction.ChangeField (field, value)

type DispatchProductAction = ProductAction -> unit

[<RequireQualifiedAccess>]
module Validate =
    open Fable.Validation.Core

    let private f = Field.key

    let newProduct (model: NewProductData) = all <| fun t ->
        {
            Id = None   // todo

            Name = t.Test (f Name) (model.Name |> Option.defaultValue "")
                |> t.Trim
                |> t.NotBlank "Please fill Product name."
                |> t.MaxLen 200 "Please make the Product name shorter, max allowed length is {len}."
                |> t.End

            Manufacturer = t.Test (f Manufacturer) (model.Manufacturer |> Option.defaultValue "")
                |> t.Trim
                |> t.NotBlank "Please fill Manufacturer name."
                |> t.MinLen 2 "Please fill Manufacturer name, name should be longer than {len} chars."
                |> t.MaxLen 200 "Please make the Manufacturer name shorter, max allowed length is {len}."
                |> t.End

            Price = None // todo

            Ean = t.TestOnlySome (f Ean) (model.Ean |> Option.map Ean.value) [
                t.Trim
                t.MaxLen 500 "Please make the Ean shorter, max allowed length is {len}."
            ] |> Option.map Dto.Common.Ean

            Links = []  // todo
        }

[<RequireQualifiedAccess>]
module ProductModel =
    let empty = {
        NewProduct = {
            Id = None
            Name = None
            Manufacturer = None
            Price = None
            Ean = None
            Links = []
        }
        Errors = Map.empty
    }

    let private clearError field model =
        { model with Errors = model.Errors.Remove field }

    let withErrors errors model =
        { model with
            Errors =
                errors
                |> Map.toList
                |> List.distinct
                |> List.fold (fun errors (field, fieldErrors) ->
                    errors.Add(Field.fromKey field, fieldErrors |> List.map ErrorMessage)
                ) model.Errors
        }

    let private updateFieldAndClearError field value model =
        field
        |> Field.update model value
        |> clearError field, Cmd.none

    let update<'ParentAction> (liftAction: ProductAction -> 'ParentAction) (model: ProductModel): ProductAction -> ProductModel * Cmd<'ParentAction> = function
        | ProductAction.ChangeField (field, value) -> model |> updateFieldAndClearError field value
