namespace MF.EDC

open System

[<AutoOpen>]
module IdModule =
    type Id = private Id of Guid

    [<RequireQualifiedAccess>]
    module Id =
        let fromGuid = Id
        let tryParse (id: string) =
            match Guid.TryParse(id) with
            | true, id -> Some (Id id)
            | _ -> None

        let create () = Guid.NewGuid() |> Id
        let value (Id id) = id |> string

//
// Size
//

[<Measure>] type Gram
[<Measure>] type Milimeter

type Weight = Weight of int<Gram>

[<RequireQualifiedAccess>]
module Weight =
    let ofGrams (weight: int) = Weight (weight * 1<Gram>)
    let grams (Weight grams) = grams
    let value = grams >> int

type Dimensions = {
    Height: int<Milimeter>
    Width: int<Milimeter>
    Length: int<Milimeter>
}

[<RequireQualifiedAccess>]
module Dimensions =
    let ofMilimeter (size: int) = size * 1<Milimeter>

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
    let parse = function
        | null | "" -> None
        | "CZK" -> Some Czk
        | "EUR" -> Some Eur
        | "USD" -> Some Usd
        | currency -> Some (Other currency)

    let value = function
        | Czk -> "CZK"
        | Eur -> "EUR"
        | Usd -> "USD"
        | Other currency -> currency

type Price = {
    Amount: float
    Currency: Currency
}

[<RequireQualifiedAccess>]
module Price =
    let create amount currency =
        {
            Amount = amount
            Currency = currency
        }

type Ean = Ean of string

[<RequireQualifiedAccess>]
module Ean =
    let value (Ean ean) = ean

type Link = Link of string
    // todo - of Uri, crete: url encode (bez mezer!)
    // - strip: fbclick, gclick, #utm

[<RequireQualifiedAccess>]
type LinkError =
    | Empty
    | IsNotWellFormed of string
    | InvalidFormat of string
    | NormalizationFailed of original: string * normalized: string
    | TooLong of normalized: string * allowed: int

[<RequireQualifiedAccess>]
module LinkError =
    let format = function
        | LinkError.Empty -> "Link is empty."
        | LinkError.IsNotWellFormed link -> sprintf "Link %A is not in well formed." link
        | LinkError.InvalidFormat link -> sprintf "Link %A is not in correct format." link
        | LinkError.NormalizationFailed (original, normalized) -> sprintf "Link %A is not normalized correctly %A." original normalized
        | LinkError.TooLong (normalized, allowedLength) -> sprintf "Link %A (%A) is longer than allowed %A chars." normalized normalized.Length allowedLength

[<RequireQualifiedAccess>]
module Link =
    open ErrorHandling

    let private clearQuery (query: string) =
        query.TrimStart('?').Split('&')
        |> Seq.toList
        |> List.filter (String.IsNullOrEmpty >> not)
        |> List.filter (String.startsWithOneOf ["gclid"; "utm_"; "fbid"] >> not)
        |> function
            | [] -> ""
            | parameters -> parameters |> String.concat "&" |> (+) "?"

    let private tryCreateUri link =
        match Uri.TryCreate(link, UriKind.Absolute) with
        | true, uri -> Some uri
        | _ -> None

    let parse = String.trim >> function
        | null | "" -> Error LinkError.Empty
        | invalid when not <| Uri.IsWellFormedUriString(invalid, UriKind.Absolute) -> Error (LinkError.IsNotWellFormed invalid)
        | link ->
            result {
                let! uri =
                    link
                    |> tryCreateUri
                    |> Result.ofOption (LinkError.InvalidFormat link)

                let query = uri.Query |> clearQuery

                let normalized =
                    sprintf "%s://%s%s%s%s%s%s"
                        uri.Scheme
                        (if uri.UserInfo |> String.IsNullOrEmpty then "" else uri.UserInfo + "@" )
                        uri.Host
                        (if uri.IsDefaultPort then "" else sprintf ":%i" uri.Port)
                        uri.AbsolutePath
                        query
                        uri.Fragment

                let! uri =
                    normalized
                    |> tryCreateUri
                    |> Result.ofOption (LinkError.NormalizationFailed (link, normalized))

                let uriString = uri.ToString()

                if uriString.Length > 500 then
                    return! Error (LinkError.TooLong (uriString, 500))

                return Link uriString
            }

    let value (Link link) = link

type Manufacturer = Manufacturer of string

[<RequireQualifiedAccess>]
module Manufacturer =
    let value (Manufacturer manufacturer) = manufacturer
    let parse = Shared.String.parse >> Option.map Manufacturer

type ProductInfo = {
    Id: Id
    Name: string
    Manufacturer: Manufacturer
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

type OwnershipStatus =
    | Own
    | Wish
    | Maybe
    | Idea
    | ToBuy
    | ToSell
    | Ordered

[<RequireQualifiedAccess>]
module OwnershipStatus =
    let parse = function
        | "Own" -> Some Own
        | "Wish" -> Some Wish
        | "Maybe" -> Some Maybe
        | "Idea" -> Some Idea
        | "ToBuy" -> Some ToBuy
        | "ToSell" -> Some ToSell
        | "Ordered" -> Some Ordered
        | _ -> None

    let value = function
        | Own -> "Own"
        | Wish -> "Wish"
        | Maybe -> "Maybe"
        | Idea -> "Idea"
        | ToBuy -> "ToBuy"
        | ToSell -> "ToSell"
        | Ordered -> "Ordered"

type Color = Color of string

[<RequireQualifiedAccess>]
module Color =
    let parse = Color >> Some
    let value (Color color) = color

type Slug = Slug of string

[<RequireQualifiedAccess>]
module Slug =
    open Slugify

    /// https://github.com/ctolkien/Slugify
    let private slugify value =
        SlugHelper().GenerateSlug(value)
        |> Slug

    let create = String.trim >> function
        | null | "" -> None
        | string -> Some (string |> slugify)

    let value (Slug slug) = slug

type TagName = TagName of string

[<RequireQualifiedAccess>]
module TagName =
    let value (TagName tag) = tag

type Tag = {
    Slug: Slug
    Name: TagName
}

[<RequireQualifiedAccess>]
module Tag =
    open ErrorHandling

    let parse: string -> Tag option = String.trim >> function
        | null | "" -> None
        | Regex @"^([a-zA-Z][a-zA-Z\-_\d]*)+$" [ tag ] when tag.Length >= 2 && tag.Length <= 30 ->
            maybe {
                let! slug = tag |> Slug.create

                return {
                    Slug = slug
                    Name = TagName tag
                }
            }
        | _ -> None

    let value ({ Name = TagName tag }: Tag) = tag

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

[<RequireQualifiedAccess>]
module CommonInfo =
    let name ({ Name = name }: CommonInfo) = name
