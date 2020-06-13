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
        let parse = function
            | null | "" -> None
            | instance -> Some (Instance instance)

        let value (Instance instance) = instance

[<RequireQualifiedAccess>]
module String =
    let trim (string: string) =
        string.Trim()

    let startsWith (prefix: string) (string: string) =
        string.StartsWith(prefix)

    let startsWithOneOf (prefixes: string list) (string: string) =
        prefixes
        |> List.exists (fun prefix -> startsWith prefix string)

    let nullable = function
        | null | "" -> null
        | string -> string
