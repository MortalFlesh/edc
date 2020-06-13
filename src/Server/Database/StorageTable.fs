namespace MF.EDC.Database

open MF.EDC

module StorageTable =
    open Microsoft.Azure.Cosmos.Table
    open Shared.FlatItem

    (* 
    Tips & Knowledge
    - @see https://tappable.co.uk/5-performance-optimising-tips-in-azure-table-storage/
    - PK is partition, which holds max of 500 entities per second -> it could be user-identificator, and it could be "his" items
    - Query
        | Point of PK * RK -> fastest
        | Row scan of PK -> still excelent performance
        | Partition scan of PK list -> it looks multiple parttions -> should be avoid!
        | Table scan -> the worst, it looks in all partittions
     *)

    type ItemDbEntity(id, (* commonInfo: Shared.Dto.Common.CommonInfo,  *)itemType: string, subType: string) =
        inherit TableEntity(
            partitionKey = itemType, // todo - create better? type-subtype?
            rowKey = id
        )

        new () = ItemDbEntity(null, null, null)

        new (item: ItemEntity) =
            let fItem = item |> Dto.Serialize.itemEntity |> FlatItem.ofItemEntity |> FlatItem.data

            // todo - not all data are save this way, specific data for any type/subType are not there
            ItemDbEntity(
                item.Id |> Id.value,
                //fItem.Common,
                fItem.Type,
                fItem.SubType |> Option.defaultValue "Other"
            )

        //member val CommonInfo = commonInfo with get, set
        member val Type = itemType with get, set
        member val SubType = subType with get, set

    [<RequireQualifiedAccess>]
    module private ItemDbEntity =
        let hydrate: ItemDbEntity -> ItemEntity option = fun dbEntity -> maybe {
            let! id =
                dbEntity.RowKey
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

            let entity = ItemDbEntity(item)
            printfn "Entity:\n%A\n------" entity
            //printfn "Entity:\n%A\n%A" entity entity.CommonInfo

            // cretae funguje, ale nejak neuklada slozitejsi data - to chce asi nejak serializovat rucne ..

            let! _ =
                itemsTable.ExecuteAsync(TableOperation.Insert(entity))
                |> AsyncResult.ofTaskCatch (fun e -> e.Message)

            return ()
        }

        let select (storageAccount: CloudStorageAccount) () = asyncResult {
            printfn "Storage.select ..."
            let! itemsTable = "Item" |> table storageAccount
            printfn "Storage.select.table ..."

            let query = TableQuery<ItemDbEntity>()
                //.Where(
                //    TableQuery.GenerateFilterCondition("PartitionKey", QueryComparisons.Equal, "Smith")
                //)
            printfn "Storage.select.table + Query ..."

            let! result =
                try itemsTable.ExecuteQuery(query) |> AsyncResult.ofSuccess
                with e ->
                    eprintf "Select Error: %A" e
                    e.Message |> AsyncResult.ofError

            let i =
                [
                    for i in result do
                        yield i
                ]
                |> List.choose ItemDbEntity.hydrate

            printfn "Data: %A" i

            printfn "Storage.select.table + Query -> RESULT! ...\n%A" result

            return
                result
                |> Seq.toList
                |> tee (fun _ -> printfn "Storage.select.result -> hydrate ...")
                |> List.choose ItemDbEntity.hydrate
                |> tee (printfn "Storage.select.result -> hydrate DONE!\n%A")

            (* return [
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
            |> List.choose ItemDbEntity.hydrate *)
        }
