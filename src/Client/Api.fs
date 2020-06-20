[<RequireQualifiedAccess>]
module Api

open Shared

module private Server =
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IEdcApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IEdcApi>

let private handleResult onSuccess onError = function
    | Ok success -> success |> onSuccess
    | Error error -> error |> onError

let private callApi<'Success, 'Action>
    (onSuccess: ('Success -> 'Action))
    (onError: (ErrorMessage -> 'Action))
    (call: AsyncResult<'Success, ErrorMessage>): Async<'Action> = async {
        let! callResult = call

        return callResult |> handleResult onSuccess onError
    }

let private handleSecuredResult onSuccess onError onAuthorizationError = function
    | Ok (token, success) ->
        token |> User.renewToken
        success |> onSuccess
    | Error (SecuredRequestError.TokenError message) ->
        User.delete ()
        message |> onAuthorizationError

    | Error (SecuredRequestError.AuthorizationError error)
    | Error (SecuredRequestError.OtherError error) ->
        error |> onError

let private callSecuredApi<'Success, 'Action>
    (onSuccess: ('Success -> 'Action))
    (onError: (ErrorMessage -> 'Action))
    (onAuthorizationError: (ErrorMessage -> 'Action))
    (call: SecuredAsyncResult<'Success, ErrorMessage>): Async<'Action> = async {
        let! callResult = call

        return callResult |> handleSecuredResult onSuccess onError onAuthorizationError
    }

//
// Public actions
//

let login onSuccess onError credentials =
    Server.api.Login credentials |> callApi onSuccess onError

let loadProfiler onLoad token = async {
    let! profiler = Server.api.LoadProfiler token

    return onLoad profiler
}

//
// Secured actions
//

let private secure onError data =
    match User.load() with
    | Some user ->
        Ok {
            Token = SecurityToken (user.Token)
            RequestData = data
        }
    | _ ->
        onError (ErrorMessage "User is not logged in.")
        |> Error

/// Compose api call with secure request creation
let inline private (>?>) secure apiCall =
    secure >> function
    | Ok secureRequest -> secureRequest |> apiCall
    | Error error -> async { return error }

(*
    Example:

let loadData onSuccess onError onAuthorizationError =
    secure onError >?> (Server.api.LoadData >> callSecuredApi onSuccess onError onAuthorizationError)
*)

let validateTag onSuccess onError =
    secure onError >?> (Server.api.ValidateTag >> callSecuredApi onSuccess onError onError)

let loadItems onSuccess onError onAuthorizationError =
    secure onError >?> (Server.api.LoadItems >> callSecuredApi onSuccess onError onAuthorizationError)

let createItem onSuccess onError onAuthorizationError =
    secure onError >?> (Server.api.CreateItem >> callSecuredApi onSuccess onError onAuthorizationError)
