namespace MF.EDC.Query

open ErrorHandling
open MF.EDC
open Shared

[<RequireQualifiedAccess>]
type UsersError =
    | Runtime of string
    | TableError of Database.CloudStorage.TableError

[<RequireQualifiedAccess>]
module UsersError =
    let format = function
        | UsersError.Runtime error -> error
        | UsersError.TableError error -> error |> Database.CloudStorage.TableError.format

type LoadUserQuery = Query<UserProfile option, UsersError>
type IsUserExistsQuery = Query<bool, UsersError>

[<RequireQualifiedAccess>]
module UsersQuery =
    open ErrorHandling.AsyncResult.Operators

    module private CloudStorage =
        open Database.CloudStorage

        let loadUser logError storageAccount userType username password =
            Table.selectUser logError storageAccount userType username password <@> UsersError.TableError

        let isUserExists logError storageAccount userType usernameOrEmail slug =
            Table.isUserExists logError storageAccount userType usernameOrEmail slug <@> UsersError.TableError

    let loadUser logError storageAccount username password: LoadUserQuery = asyncResult {
        let! user =
            CloudStorage.loadUser logError storageAccount UserType.Registered username password
            >>@ (UsersError.format >> eprintfn "StorageTable load failed! %A")  // todo - log error

        return user
    }

    let isUserExists logError storageAccount slug usernameOrEmail: IsUserExistsQuery = asyncResult {
        let! user =
            CloudStorage.isUserExists logError storageAccount UserType.Registered usernameOrEmail slug
            >>@ (UsersError.format >> eprintfn "StorageTable load failed! %A")  // todo - log error

        return user
    }
