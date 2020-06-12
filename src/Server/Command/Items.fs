namespace MF.EDC.Command

open ErrorHandling
open MF.EDC

type CreateItemCommand = Command<ItemEntity, ItemEntity, string>

[<RequireQualifiedAccess>]
module ItemsCommand =

    module private StorageTable =
        open Microsoft.WindowsAzure.Storage
        open Database.StorageTable

        let create (storageAccount: CloudStorageAccount) (item: ItemEntity) =
            item
            |> Table.insert storageAccount
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

    let create storageAccount mysqlLocalConnection azureSqlConnection: CreateItemCommand = fun item -> asyncResult {
        let! results =
            [
                item |> StorageTable.create storageAccount
                item |> MySql.create mysqlLocalConnection
                item |> AzureSql.create azureSqlConnection
            ]
            |> Async.Parallel
            |> AsyncResult.ofAsyncCatch (fun e -> e.Message)

        let! _ =
            results
            |> Seq.toList
            |> Result.sequence
            |> AsyncResult.ofResult

        return item
    }
