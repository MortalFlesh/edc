namespace Shared

type SuccessMessage = SuccessMessage of string
type ErrorMessage = ErrorMessage of string

[<RequireQualifiedAccess>]
module ErrorMessage =
    let fromExn (e: exn) =
        ErrorMessage e.Message

    let value (ErrorMessage errorMessage) = errorMessage

[<RequireQualifiedAccess>]
type Message =
    | Success of SuccessMessage
    | Error of ErrorMessage

//
// Security
//

type Username = Username of string
type Password = Password of string

[<RequireQualifiedAccess>]
module Username =
    let empty = Username ""
    let value (Username username) = username

[<RequireQualifiedAccess>]
module Password =
    let empty = Password ""
    let value (Password password) = password

type JWTToken = JWTToken of string

[<RequireQualifiedAccess>]
module JWTToken =
    let value (JWTToken value) = value

//
// DTO
//

[<RequireQualifiedAccess>]
module Dto =
    open System

    module Login =
        type User = {
            Username: Username
            Token: JWTToken
        }

    module Common =
        //
        // Size
        //

        [<Measure>] type Gram
        [<Measure>] type Milimeter

        type Weight = Weight of int<Gram>

        [<RequireQualifiedAccess>]
        module Weight =
            let ofGrams grams = Weight (grams * 1<Gram>)

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

        type Id = Id of string

        [<RequireQualifiedAccess>]
        module Id =
            let parse = function
                | null | "" -> None
                | id -> Some (Id id)

            let value (Id id) = id

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
            Tags: Tag list
            Links: Link list
            Price: Price option
            Size: Size option
            OwnershipStatus: OwnershipStatus
            Product: ProductInfo option
            Gallery: Gallery option
        }

    module Items =
        open Common

        //
        // Tools
        //

        type ToolInfo = {
            Common: CommonInfo
        }

        type Tool =
            | MultiTool of ToolInfo
            | Knife of ToolInfo
            | Gun of ToolInfo
            | Other of ToolInfo

        //
        // Consumables
        //

        type ConsumableInfo = {
            Common: CommonInfo
        }

        type Consumable =
            | Food of ConsumableInfo
            | Other of ConsumableInfo

        //
        // Items
        //

        [<RequireQualifiedAccess>]
        type Item =
            | Tool of Tool
            | Container of Container
            | Consumable of Consumable

        //
        // Containers
        //

        and Container =
            | BagPack of ContainerInfo
            | Organizer of ContainerInfo
            | Pocket of ContainerInfo
            | Panel of ContainerInfo
            | Other of ContainerInfo

        and ContainerInfo = {
            Common: CommonInfo

            Items: ItemInContainer list
            TotalSize: Size
        }

        and ItemInContainer = {
            Item: ItemEntity
            Quantity: int
        }

        //
        // Entities
        //

        and ItemEntity = {
            Id: Id
            Item: Item
        }

        type ContainerEntity = {
            Id: Id
            Container: Container
        }

        [<RequireQualifiedAccess>]
        module ItemEntity =
            let item: ItemEntity -> Item = fun { Item = item } -> item

    module Edc =
        open Common
        open Items

        type EDCSet = {
            Id: Id
            Name: string option
            Description: string option
            Inventory: ContainerEntity list
        }

//
// Routing, etc.
//

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

//
// Debug mode
//

[<RequireQualifiedAccess>]
module Profiler =
    type Token = Token of string

    type Id = Id of string
    type Label = Label of string
    type Value = Value of string
    type ValueDetail = ValueDetail of string
    type Unit = Unit of string
    type Link = Link of string

    type Color =
        | Yellow
        | Green
        | Red
        | Gray

    type Status = {
        Color: Color option
        Value: Value    // todo - this should be something like Icon
    }

    type DetailItem = {
        ShortLabel: Label option
        Label: Label
        Detail: ValueDetail option
        Value: Value
        Color: Color option
        Link: Link option
    }

    [<RequireQualifiedAccess>]
    module Detail =
        let createItem label value =
            {
                ShortLabel = None
                Label = label
                Detail = None
                Value = value
                Color = None
                Link = None
            }

        let addLink: Link -> DetailItem -> DetailItem = fun link item -> { item with Link = Some link }
        let addColor: Color -> DetailItem -> DetailItem = fun color item -> { item with Color = Some color }

    type Item = {
        Id: Id
        Label: Label option
        Value: Value
        ItemColor: Color option
        Unit: Unit option
        StatusIcon: Status option
        Detail: DetailItem list
    }

    type Toolbar = Toolbar of Item list

    let emptyValue = Value ""

//
// Api & Requests
//

open Dto.Login
open Dto.Items
open Dto.Edc

type AsyncResult<'Success, 'Error> = Async<Result<'Success, 'Error>>

type SecurityToken = SecurityToken of JWTToken
type RenewedToken = RenewedToken of JWTToken

type SecureRequest<'RequestData> = {
    Token: SecurityToken
    RequestData: 'RequestData
}

[<RequireQualifiedAccess>]
type SecuredRequestError<'Error> =
    | TokenError of 'Error
    | AuthorizationError of 'Error
    | OtherError of 'Error

type SecuredAsyncResult<'Success, 'Error> = AsyncResult<RenewedToken * 'Success, SecuredRequestError<'Error>>

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type IEdcApi = {
    // Public actions
    Login: Username * Password -> AsyncResult<User, ErrorMessage>

    LoadProfiler: Profiler.Token option -> Async<Profiler.Toolbar option>

    // Secured actions
    (*
        Example:

        LoadData: SecureRequest<RequestData> -> SecuredAsyncResult<SecuredData, ErrorMessage>
    *)
    LoadItems: SecureRequest<unit> -> SecuredAsyncResult<ItemEntity list, ErrorMessage>
}
