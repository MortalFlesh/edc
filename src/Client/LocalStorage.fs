[<RequireQualifiedAccess>]
module LocalStorage

open Thoth.Json

type Key =
    | User
    | ProfilerToken

[<RequireQualifiedAccess>]
module Key =
    let value = function
        | User -> "user"
        | ProfilerToken -> "profiler-token"

let load (decoder: Decoder<'T>) key: Result<'T,string> =
    let key = key |> Key.value
    let o = Browser.WebStorage.localStorage.getItem key
    if isNull o then
        "No item found in local storage with key " + key |> Error
    else
        Decode.fromString decoder o

let delete key =
    let key = key |> Key.value
    Browser.WebStorage.localStorage.removeItem(key)

let inline save key (data: 'T) =
    let key = key |> Key.value
    Browser.WebStorage.localStorage.setItem(key, Encode.Auto.toString(0, data))
