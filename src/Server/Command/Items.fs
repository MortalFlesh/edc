namespace MF.EDC.Command

open ErrorHandling
open MF.EDC

type CreateItemCommand = Command<ItemEntity, ItemEntity, string>

[<RequireQualifiedAccess>]
module ItemsCommand =

    module private StorageTable =
        open Microsoft.Azure.Cosmos.Table
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

    open ErrorHandling.AsyncResult.Operators

    let create storageAccount mysqlLocalConnection azureSqlConnection: CreateItemCommand = fun item -> asyncResult {
        printfn "=========================\nCreate item\n========================="
        let! results =
            [
                item |> StorageTable.create storageAccount >>* (fun _ -> printfn "StorageTable create done!") >>@ (eprintfn "StorageTable.Error: %A")
                //item |> MySql.create mysqlLocalConnection >>* (fun _ -> printfn "MySql create done!") >>@ (eprintfn "MySql.Error: %A")
                //item |> AzureSql.create azureSqlConnection >>* (fun _ -> printfn "AzureSql create done!") >>@ (eprintfn "AzureSql.Error: %A")
            ]
            |> Async.Parallel
            |> AsyncResult.ofAsyncCatch (fun e -> e.Message)
        printfn "=========================\nDone\n========================="

        let! _ =
            results
            |> Seq.toList
            |> Result.sequence
            |> AsyncResult.ofResult

        return item
    }
