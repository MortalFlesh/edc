[<RequireQualifiedAccess>]
module Product

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

open ProductModule

let form onSubmit isSubmitting (dispatch: DispatchProductAction) (model: ProductModel) =
    let productModel = model.NewProduct

    let fieldErrors field =
        model.Errors
        |> Map.tryFind field
        |> Option.defaultValue []

    let inputField input field =
        Component.inputField
            "Product"
            onSubmit
            (not isSubmitting)
            input
            (ProductAction.changeField field >> dispatch)
            (field |> Field.title)
            (field |> fieldErrors)

    div [] [
        // todo show global Form errors (Field.Form)

        productModel.Name
        |> Option.defaultValue ""
        |> inputField Input.text Name

        productModel.Manufacturer
        |> Option.defaultValue ""
        |> inputField Input.text Manufacturer

        productModel.Price
        |> Option.defaultValue ""
        |> inputField Input.text Price

        productModel.Ean
        |> Option.map Ean.value
        |> Option.defaultValue ""
        |> inputField Input.text Ean

        (* productModel.Links
        |> Option.defaultValue ""
        |> inputField Input.text Links *)
    ]
