namespace MF.EDC

open System

//
// Size
//

[<Measure>] type Gram
[<Measure>] type Milimeter

type Weight = Weight of int<Gram>

[<RequireQualifiedAccess>]
module Weight =
    let grams (Weight grams) = grams
    let value = grams >> int

type Dimensions = {
    Height: int<Milimeter>
    Width: int<Milimeter>
    Length: int<Milimeter>
}

type Size = {
    Weight: Weight option
    Dimensions: Dimensions option
}

//
// Product
//

type Currency =
    | Czk
    | Eur
    | Usd
    | Other of string

[<RequireQualifiedAccess>]
module Currency =
    let value = function
        | Czk -> "CZK"
        | Eur -> "EUR"
        | Usd -> "USD"
        | Other currency -> currency

type Price = {
    Amount: float
    Currency: Currency
}

type Ean = Ean of string

[<RequireQualifiedAccess>]
module Ean =
    let value (Ean ean) = ean

type Link = Link of string

[<RequireQualifiedAccess>]
module Link =
    let value (Link link) = link

type ProductInfo = {
    Name: string
    Price: Price
    Ean: Ean option
    Links: Link list
}

//
// Gallery
//

type Gallery = {
    Images: Link list
}

//
// Common
//

[<AutoOpen>]
module IdModule =
    type Id = private Id of Guid

    [<RequireQualifiedAccess>]
    module Id =
        let fromGuid = Id
        let create () = Guid.NewGuid() |> Id
        let value (Id id) = id |> string

type OwnershipStatus =
    | Own
    | Wish
    | Maybe
    | Idea
    | ToBuy
    | ToSell
    | Ordered

type Color = Color of string

[<RequireQualifiedAccess>]
module Color =
    let value (Color color) = color

type Tag = Tag of string

[<RequireQualifiedAccess>]
module Tag =
    let value (Tag tag) = tag

type CommonInfo = {
    Name: string
    Note: string option
    Color: Color option
    Tags: Tag list
    Links: Link list
    Price: Price option
    Size: Size option
    OwnershipStatus: OwnershipStatus
    Product: ProductInfo option
    Gallery: Gallery option
}
