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

    module private MySql =
        open Database.MySql

        let create connection (item: ItemEntity) =
            { Id = item.Id |> Id.value; Name = item |> ItemEntity.name }
            |> MySql.insert connection

    module private AzureSql =
        open Database.AzureSql

        let create connection (item: ItemEntity) =
            { Id = item.Id |> Id.value; Name = item |> ItemEntity.name }
            |> AzureSql.insert connection

    open ErrorHandling.AsyncResult.Operators

    let create storageAccount mysqlLocalConnection azureSqlConnection username: CreateItemCommand = fun item -> asyncResult {
        let itemEntity = {
            Id = Id.create()
            Item = item
        }

        let! results =
            [
                itemEntity |> StorageTable.createForUser storageAccount username <@> Database.CloudStorage.TableError.format >>@ (eprintfn "StorageTable.Error: %A")  // todo logError
                //item |> MySql.create mysqlLocalConnection >>* (fun _ -> printfn "MySql create done!") >>@ (eprintfn "MySql.Error: %A")
                //item |> AzureSql.create azureSqlConnection >>* (fun _ -> printfn "AzureSql create done!") >>@ (eprintfn "AzureSql.Error: %A")
            ]
            |> Async.Parallel
            |> AsyncResult.ofAsyncCatch (fun e -> e.Message)

        let! _ =
            results
            |> Seq.toList
            |> Result.sequence
            |> AsyncResult.ofResult

        return itemEntity
    }
