[<RequireQualifiedAccess>]
module User

open Thoth.Json
open Shared
open Shared.Dto.Login

let save (user: User) =
    user |> LocalStorage.save LocalStorage.Key.User

let load (): User option =
    let userDecoder = Decode.Auto.generateDecoder<User>()

    match LocalStorage.load userDecoder LocalStorage.Key.User with
    | Ok user -> Some user
    | Error _ ->
        LocalStorage.delete LocalStorage.Key.User
        None   // todo - log error

let renewToken (RenewedToken token) =
    match load() with
    | Some user -> { user with Token = token } |> save
    | _ -> ()

let delete () =
    LocalStorage.delete LocalStorage.Key.User
