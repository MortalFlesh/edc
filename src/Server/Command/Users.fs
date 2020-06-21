namespace MF.EDC.Command

open ErrorHandling
open Shared
open MF.EDC

type CreateUserCommand = Command<Username * Password * Email * Slug, UserProfile, string>

[<RequireQualifiedAccess>]
module UsersCommand =

    module private StorageTable =
        open Microsoft.Azure.Cosmos.Table
        open Database.CloudStorage

        let createUser (storageAccount: CloudStorageAccount) userType password (userProfile: UserProfile) =
            userProfile
            |> Table.insertUser storageAccount userType password
            |> AsyncResult.map ignore

    open ErrorHandling.AsyncResult.Operators

    let create storageAccount: CreateUserCommand = fun (username, password, email, slug) -> asyncResult {
        let userProfile = {
            Id = Id.create()
            Username = username
            Email = email
            Slug = slug
        }

        do!
            userProfile
            |> StorageTable.createUser storageAccount UserType.Registered (password |> Password.encrypt)
            <@> Database.CloudStorage.TableError.format >>@ (eprintfn "StorageTable.Error: %A")  // todo logError

        return userProfile
    }
