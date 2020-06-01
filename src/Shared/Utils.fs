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

[<RequireQualifiedAccess>]
module Result =
    let toOption = function
        | Ok value -> Some value
        | _ -> None
