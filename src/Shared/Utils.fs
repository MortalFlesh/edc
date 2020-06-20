namespace Shared

[<AutoOpen>]
module Utils =
    let tee f a =
        f a
        a

    let (=>) key value = key, value

    let className: string list -> string = String.concat " "

[<RequireQualifiedAccess>]
module List =
    /// see https://stackoverflow.com/questions/32363848/fastest-way-to-reduce-a-list-based-on-another-list-using-f
    let filterNotIn excluding list =
        let toExclude = set excluding
        list |> List.filter (toExclude.Contains >> not)

    let filterNotInBy f excluding list =
        let toExclude = set excluding
        list |> List.filter (f >> toExclude.Contains >> not)

    let filterInBy f including list =
        let toInclude = set including
        list |> List.filter (f >> toInclude.Contains)

    let takeUpTo limit list =
        if list |> List.length <= limit then list
        else list |> List.take limit

[<RequireQualifiedAccess>]
module String =
    let countWords (string: string) =
        string.Split ' ' |> Seq.length

    let split (delimiter: char) (string: string) =
        string.Split [| delimiter |]

    let toLower (string: string) =
        string.ToLower()

    let parse = function
        | null | "" -> None
        | string -> Some string

    let trim (string: string) = string.Trim()

[<RequireQualifiedAccess>]
module Result =
    let toOption = function
        | Ok value -> Some value
        | _ -> None

    let combineErrorMaps r1 r2 =
        match r1, r2 with
        | Ok s1, Ok s2 -> Ok (s1, s2)
        | Error errors1, Error errors2 ->
            (errors1 |> Map.toList) @ (errors2 |> Map.toList)
            |> List.distinct
            |> Map.ofList
            |> Error
        | Error e, _
        | _, Error e -> Error e
