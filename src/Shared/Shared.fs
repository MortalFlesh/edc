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
    module Login =
        type User = {
            Username: Username
            Token: JWTToken
        }

    module Edc =
        type EdcSet = EdcSet of string

        [<RequireQualifiedAccess>]
        module EdcSet =
            let parse = function
                | null | "" -> None
                | set -> Some (EdcSet set)

            let value (EdcSet set) = set

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
}
