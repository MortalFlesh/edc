namespace MF.EDC.Database

open MF.EDC

module StorageTable =
    open Microsoft.WindowsAzure
    open Microsoft.WindowsAzure.Storage
    open Microsoft.WindowsAzure.Storage.Table

    open Shared.FlatItem

    type ItemDbEntity(id, name, commonInfo: Shared.Dto.Common.CommonInfo, itemType: string, subType: string) =
        inherit TableEntity(
            partitionKey = (id |> Id.value),
            rowKey = name
        )

        new () = ItemDbEntity()

        new (item: ItemEntity) =
            let fItem = item |> Dto.Serialize.itemEntity |> FlatItem.ofItemEntity |> FlatItem.data

            // todo - not all data are save this way, specific data for any type/subType are not there
            ItemDbEntity(
                item.Id,
                fItem.Common.Name,
                fItem.Common,
                fItem.Type,
                fItem.SubType |> Option.defaultValue "Other"
            )

        member val CommonInfo = commonInfo with get, set
        member val Type = itemType with get, set
        member val SubType = subType with get, set

    [<RequireQualifiedAccess>]
    module private ItemDbEntity =
        let hydrate: ItemDbEntity -> ItemEntity option = fun dbEntity -> maybe {
            let! id =
                dbEntity.PartitionKey
                |> Id.tryParse

            //let common = dbEntity.CommonInfo |> Dto.Deserialize.commonInfo // todo ...

            return {
                Id = id
                Item = Item.Tool (Knife {
                    Common = {
                        Name = dbEntity.RowKey
                        Note = Some "AzureStorage"
                        OwnershipStatus = Own
                        Color = None; Tags = []; Links = []; Price = None; Size = None ; Product = None; Gallery = None
                    }
                })
            }
        }

    module Table =
        open ErrorHandling

        // https://docs.microsoft.com/cs-cz/dotnet/fsharp/using-fsharp-on-azure/table-storage

        let private table (storageAccount: CloudStorageAccount) tableName = asyncResult {
            let tableClient = storageAccount.CreateCloudTableClient()
            let itemsTable = tableClient.GetTableReference(tableName)

            let! _ =
                itemsTable.CreateIfNotExistsAsync()
                |> AsyncResult.ofTaskCatch (fun e -> e.Message)

            return itemsTable
        }

        let insert (storageAccount: CloudStorageAccount) (item: ItemEntity) = asyncResult {  // todo - create InsertError
            let! itemsTable = "Item" |> table storageAccount

            let! _ =
                itemsTable.ExecuteAsync(TableOperation.Insert(ItemDbEntity(item)))
                |> AsyncResult.ofTaskCatch (fun e -> e.Message)

            return ()
        }

        let select (storageAccount: CloudStorageAccount) () = asyncResult {
            let! itemsTable = "Item" |> table storageAccount

            let query = TableQuery<ItemDbEntity>()
                //.Where(
                //    TableQuery.GenerateFilterCondition("PartitionKey", QueryComparisons.Equal, "Smith")
                //)

            return [
                let mutable token: TableContinuationToken = null
                while token |> isNull do
                    let result =
                        itemsTable.ExecuteQuerySegmentedAsync<ItemDbEntity>(query, token)
                        |> AsyncResult.ofTaskCatch ignore
                        |> Async.RunSynchronously

                    match result with
                    | Ok queryResult ->
                        token <- queryResult.ContinuationToken
                        yield! queryResult.Results

                    | _ -> ()
            ]
            |> List.choose ItemDbEntity.hydrate
        }
