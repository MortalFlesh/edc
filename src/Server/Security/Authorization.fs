namespace MF.EDC

open Shared
open ErrorHandling

[<RequireQualifiedAccess>]
module Authorize =
    open ErrorHandling.Result.Operators

    [<RequireQualifiedAccess>]
    module private SecureRequest =
        let accessData assertGranted { Token = token; RequestData = data } = result {
            let! renewedToken = assertGranted token

            return renewedToken, data
        }

        let access assertGranted { Token = token; RequestData = data } = result {
            let! renewedToken = assertGranted token data

            return renewedToken, data
        }

    let private isGranted instance appKey keysForToken permission (SecurityToken token) =
        permission
        |> JWTToken.isGranted instance keysForToken token
        <!> (JWTToken.renew appKey >> RenewedToken)

    type Authorize<'RequestData> = Instance -> JWTKey -> JWTKey list -> SecureRequest<'RequestData> -> Result<RenewedToken * 'RequestData, AuthorizationError>

    let withLogin: Authorize<'RequestData> =
        fun instance appKey keysForToken ->
            SecureRequest.accessData (isGranted instance appKey keysForToken ValidToken)

    open ErrorHandling.AsyncResult.Operators

    [<RequireQualifiedAccess>]
    module private AuthorizationError =
        let format logError = function
            | JwtValidationError (JwtValidationError.Unexpected e) ->
                logError <| sprintf "Unexpected authorization error.\n%s" e.Message
                "Unexpected authorization error." |> ErrorMessage |> SecuredRequestError.TokenError

            | JwtValidationError MissingKeyData ->
                logError "Missing key for JWT validation."
                "Unexpected authorization error." |> ErrorMessage |> SecuredRequestError.TokenError

            | JwtValidationError detail ->
                sprintf "Action is not granted! %A" detail |> ErrorMessage |> SecuredRequestError.TokenError

            | ActionIsNotGranted detail ->
                sprintf "Action is not granted! %s" detail |> ErrorMessage |> SecuredRequestError.AuthorizationError

            | RequestError (ErrorMessage error) ->
                error |> ErrorMessage |> SecuredRequestError.AuthorizationError

    /// Helper function to create an Operator for easier authorization
    let authorizeAction
        (appInstance: Instance)
        (appKey: JWTKey)
        (keysForToken: JWTKey list)
        (logAuthorizationError: string -> unit)
        (authorize: Authorize<'RequestData>)
        (action: 'RequestData -> AsyncResult<'ResponseData, ErrorMessage>)
        (request: SecureRequest<'RequestData>): AsyncResult<RenewedToken * 'ResponseData, SecuredRequestError<ErrorMessage>>
        = asyncResult {
            let! (renewedToken, requestData) =
                request
                |> authorize appInstance appKey keysForToken
                |> AsyncResult.ofResult <@> (AuthorizationError.format logAuthorizationError)

            let! response =
                requestData
                |> action <@> SecuredRequestError.OtherError

            return renewedToken, response
        }
