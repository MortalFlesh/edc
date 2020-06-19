namespace MF.EDC

open Shared
open ErrorHandling

//
// Errors
//

type JwtValidationError =
    | MissingKeyData
    | Unexpected of exn
    | TokenStatus of string
    | MissingUsername
    | MissingDisplayName
    | MissingGroups

type AuthorizationError =
    | JwtValidationError of JwtValidationError
    | ActionIsNotGranted of string
    | RequestError of ErrorMessage

//
// Types
//

[<RequireQualifiedAccess>]
module UserCustomData =
    let [<Literal>] Username = "username"
    let [<Literal>] DisplayName = "user"
    let [<Literal>] Groups = "groups"

[<RequireQualifiedAccess>]
type CustomItem =
    | String of string * string
    | Strings of string * (string list)

[<AutoOpen>]
module JwtKeyModule =
    type JWTKey =
        private
        | JWTKey of System.Guid

    [<RequireQualifiedAccess>]
    module JWTKey =
        open System

        let forDevelopment = JWTKey ("97a5a8b4-dea4-4b46-bd11-5b003bf304b0" |> Guid.Parse)

        let generate () =
            Guid.NewGuid()
            |> JWTKey

        let value = function
            | JWTKey key -> key.ToString()

type PermissionGroup = PermissionGroup of string

[<RequireQualifiedAccess>]
module PermissionGroup =
    let value (PermissionGroup group) = group

[<RequireQualifiedAccess>]
module Username =
    let value (Username username) = username

type Permission =
    | ValidToken
    | Group of PermissionGroup

[<RequireQualifiedAccess>]
module JWTToken =
    open System
    open JsonWebToken
    open ErrorHandling.Result.Operators

    type private GrantedToken = GrantedToken of Jwt

    type private UserData = {
        Username: Username
        DisplayName: string
        Groups: PermissionGroup list
        GrantedToken: GrantedToken
    }

    type GrantedTokenData = private GrantedTokenData of UserData

    let value (JWTToken value) = value

    let private readUserData currentApp key (JWTToken token) =
        try
            use key = new SymmetricJwk(key |> JWTKey.value)
            let currentInstance = currentApp |> Instance.value

            let policy =
                TokenValidationPolicyBuilder()
                    .RequireSignature(key, SignatureAlgorithm.HmacSha256)
                    .RequireIssuer(currentInstance)
                    .RequireAudience(currentInstance)
                    .EnableLifetimeValidation(true, 10)
                    .Build()

            let jwtResult = JwtReader().TryReadToken(token, policy)

            if jwtResult.Succedeed then
                result {
                    let! username =
                        match jwtResult.Token.Payload.TryGetValue(UserCustomData.Username) with
                        | true, username -> Ok (username.Value.ToString())
                        | _ -> Error MissingUsername

                    let! displayName =
                        match jwtResult.Token.Payload.TryGetValue(UserCustomData.DisplayName) with
                        | true, user -> Ok (user.Value.ToString())
                        | _ -> Error MissingDisplayName

                    let! groups =
                        match jwtResult.Token.Payload.TryGetValue(UserCustomData.Groups) with
                        | true, groups ->
                            match groups.Value with
                            | :? JwtArray as groups ->
                                groups
                                |> Seq.map (fun i -> PermissionGroup (i.Value.ToString()))
                                |> Seq.toList
                                |> Ok
                            | _ -> Error MissingGroups
                        | _ -> Error MissingGroups

                    return GrantedTokenData {
                        Username = Username username
                        DisplayName = displayName
                        Groups = groups
                        GrantedToken = GrantedToken jwtResult.Token
                    }
                }
            else
                jwtResult.Status.ToString()
                |> TokenStatus
                |> Error
        with
        | e -> Error (Unexpected e)

    let isGranted currentApp keysForToken token requiredPermission = result {
        let allUserData =
            keysForToken
            |> List.map (fun key ->
                token
                |> readUserData currentApp key
                <@> JwtValidationError
            )

        let! userData =
            match allUserData with
            | [] ->
                Error (JwtValidationError MissingKeyData)

            | onlyErrors when onlyErrors |> List.forall (function | Error _ -> true | _ -> false) ->
                onlyErrors
                |> List.head

            | atLeastOneGranted when atLeastOneGranted |> List.exists (function | Ok _ -> true | _ -> false) ->
                atLeastOneGranted
                |> List.pick (function
                    | Ok data -> Some (Ok data)
                    | _ -> None
                )

            | firstError ->
                firstError
                |> List.pick (function
                    | Error error -> Some (Error error)
                    | _ -> None
                )

        return!
            match requiredPermission with
            | ValidToken -> Ok userData
            | Group requiredGroup ->
                match userData with
                | GrantedTokenData { Groups = groups } when groups |> List.exists ((=) requiredGroup) -> Ok userData
                | _ -> Error (ActionIsNotGranted "You are not authorized for this action.")
    }

    let rec private addCustomData customData (descriptor: JwsDescriptor) =
        match customData with
        | [] -> descriptor

        | CustomItem.String (key, value) :: rest ->
            descriptor.AddClaim(key, value)
            descriptor |> addCustomData rest

        | CustomItem.Strings (key, values) :: rest ->
            let jwtValue (value: string) = JwtValue(value)

            let array =
                values
                |> List.map jwtValue
                |> List.toGeneric
                |> JwtArray

            descriptor.AddClaim(key, array)
            descriptor |> addCustomData rest

    let create currentApp appKey customData =
        use key = new SymmetricJwk(appKey |> JWTKey.value, SignatureAlgorithm.HmacSha256)
        let currentInstance = currentApp |> Instance.value
        let now = DateTime.UtcNow

        JwsDescriptor(
            SigningKey = key,
            JwtId = Guid.NewGuid().ToString(),
            IssuedAt = (now |> Nullable),
            NotBefore = (now |> Nullable),
            ExpirationTime = (now.AddMinutes(30.0) |> Nullable),
            Issuer = currentInstance,
            Audience = currentInstance
        )
        |> addCustomData customData
        |> JwtWriter().WriteTokenString
        |> JWTToken

    let renew appKey (GrantedTokenData userData) =
        use key = new SymmetricJwk(appKey |> JWTKey.value, SignatureAlgorithm.HmacSha256)

        let (GrantedToken token) = userData.GrantedToken
        let customData = [
            CustomItem.String (UserCustomData.Username, userData.Username |> Username.value)
            CustomItem.String (UserCustomData.DisplayName, userData.DisplayName)
            CustomItem.Strings (UserCustomData.Groups, userData.Groups |> List.map PermissionGroup.value)
        ]

        JwsDescriptor(
            SigningKey = key,
            JwtId = Guid.NewGuid().ToString(),
            IssuedAt = token.IssuedAt,
            NotBefore = token.NotBefore,
            ExpirationTime = (DateTime.UtcNow.AddMinutes(30.0) |> Nullable),
            Issuer = token.Issuer,
            Audience = (token.Audiences |> Seq.head)
        )
        |> addCustomData customData
        |> JwtWriter().WriteTokenString
        |> JWTToken

    let userData currentApp key (RenewedToken token): Result<User, _> = result {
        let! (GrantedTokenData user) = token |> readUserData currentApp key

        return {
            Username = user.Username
            Token = token
        }
    }
