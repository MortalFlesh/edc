namespace MF.EDC

[<AutoOpen>]
module Utils =
    let tee f a =
        f a
        a

    let (=>) key value = key, value

[<AutoOpen>]
module Regexp =
    open System.Text.RegularExpressions

    // http://www.fssnip.net/29/title/Regular-expression-active-pattern
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ])
        else None

[<RequireQualifiedAccess>]
module FileSystem =
    open System.IO

    let tryReadContent (filePath: string) =
        if File.Exists filePath then File.ReadAllText(filePath) |> Some
        else None

[<AutoOpen>]
module Options =
    type MaybeBuilder () =
        member __.Bind(o, f) =
            match o with
            | None -> None
            | Some a -> f a

        member __.Return(x) = Some x
        member __.ReturnFrom(x) = x

    let maybe = MaybeBuilder()

[<RequireQualifiedAccess>]
type TimeoutError =
    | Timeouted
    | Error of exn

[<RequireQualifiedAccess>]
module TimeoutError =
    let format = function
        | TimeoutError.Timeouted -> "Request ends by timeout."
        | TimeoutError.Error e -> sprintf "Request ends with error %A." e.Message

[<RequireQualifiedAccess>]
module Async =
    let runSynchronouslyWithTimeout millisecondsDueTime action =
        try
            Async.RunSynchronously(action, millisecondsDueTime) |> Ok
        with
        | :? System.TimeoutException -> TimeoutError.Timeouted |> Error
        | e -> TimeoutError.Error e |> Error

[<RequireQualifiedAccess>]
module List =
    let toGeneric (list: _ list): System.Collections.Generic.List<_> =
        list
        |> System.Linq.Enumerable.ToList

[<AutoOpen>]
module InstanceModule =
    type Instance = private Instance of string

    [<RequireQualifiedAccess>]
    module Instance =
        /// Todo - remove - now just for debuggging
        let create = Instance

        let parse = function
            | null | "" -> None
            | instance -> Some (Instance instance)

        let value (Instance instance) = instance
