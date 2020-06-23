namespace MF.EDC.Command

open ErrorHandling
open MF.EDC

type CreateItemCommand = Command<Item, ItemEntity, string>

[<RequireQualifiedAccess>]
module ItemsCommand =

    module private StorageTable =
        open Microsoft.Azure.Cosmos.Table
        open Database.CloudStorage

        let createForUser (storageAccount: CloudStorageAccount) username (item: ItemEntity) =
            item
            |> Table.insertItem storageAccount (Owner.User username)
            |> AsyncResult.map ignore

    open ErrorHandling.AsyncResult.Operators

    let create storageAccount username: CreateItemCommand = fun item -> asyncResult {
        let itemEntity = {
            Id = Id.create()
            Item = item
        }

        do!
            itemEntity
            |> StorageTable.createForUser storageAccount username <@> Database.CloudStorage.TableError.format >>@ (eprintfn "StorageTable.Error: %A")  // todo logError

        return itemEntity
    }
