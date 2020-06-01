namespace ErrorHandling

[<RequireQualifiedAccess>]
module Option =
    open System

    let retn x = Some x

    let mapNone f = function
        | Some v -> Some v
        | None -> f ()

    let ofChoice = function
        | Choice1Of2 x -> Some x
        | _ -> None

    let toChoice case2 = function
        | Some x -> Choice1Of2 x
        | None -> Choice2Of2 (case2 ())

    let ofNullable (nullable: Nullable<'a>): 'a option =
        match box nullable with
        | null -> None // CLR null
        | :? Nullable<'a> as n when not n.HasValue -> None // CLR struct
        | :? Nullable<'a> as n when n.HasValue -> Some (n.Value) // CLR struct
        | x when x.Equals (DBNull.Value) -> None // useful when reading from the db into F#
        | x -> Some (unbox x) // anything else

    let toNullable = function
        | Some item -> new Nullable<_>(item)
        | None -> new Nullable<_>()

    let orDefault x = function
        | None -> x ()
        | Some y -> y

    let tee f = function
        | Some x -> f x; Some x
        | None -> None

    let teeNone f = function
        | Some x -> Some x
        | None -> f(); None

    let toResult = function
        | Some (Ok success) -> Ok (Some success)
        | Some (Error error) -> Error error
        | None -> Ok None

    module Operators =
        /// Option.bind
        let inline (>>=) o f = Option.bind f o

        /// Option.tee
        let inline (>>*) o f = tee f o

        /// Option.teeNone
        let inline (>>@) o f = teeNone f o

        /// Option.map
        let inline (<!>) o f = Option.map f o

        /// Option.mapNone
        let inline (<@>) o f = mapNone f o

        /// Option.defaultValue - if value is None, default value will be used
        let (<?=>) defaultValue o = Option.defaultValue o defaultValue

        /// Option.orElse - if value is None, other option will be used
        let (<??>) other o = Option.orElse o other

        /// Result.ofOption - if value is None, error will be returned
        let (<?!>) o error = o |> Result.ofOption error

        /// Option.iter
        let (|>!) o f = o |> Option.iter f
