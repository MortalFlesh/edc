namespace MF.EDC

open System

//
// Size
//

[<Measure>] type Gram
[<Measure>] type Milimeter

type Weight = Weight of int<Gram>

type Dimensions = {
    Heigh: int<Milimeter>
    Width: int<Milimeter>
    Depth: int<Milimeter>
}

type Size = {
    Weight: Weight option
    Dimensions: Dimensions option
}

//
// Product
//

type Price = {
    Amount: float
    Currency: string
}

type Ean = Ean of string

type Link = Link of string

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

type Id = Id of Guid

type OwnershipStatus =
    | Own
    | Wish
    | Maybe
    | Idea
    | ToBuy
    | ToSell
    | Ordered

type Color = Color of string

type Tag = Tag of string

type CommonInfo = {
    Name: string
    Note: string option
    Color: Color option
    Tag: Tag list
    Links: Link list
    Price: Price option
    Size: Size option
    OwnershipStatus: OwnershipStatus
    Product: ProductInfo option
    Gallery: Gallery
}
