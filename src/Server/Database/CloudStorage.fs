namespace MF.EDC.Database

open MF.EDC

module CloudStorage =
    open Microsoft.Azure.Cosmos.Table
    open FlatItem
    open ErrorHandling

    //
    // Common constants
    //
    [<RequireQualifiedAccess>]
    type TableName =
        | Item
        | Tag
        | Product
        | Set
        | User

    [<RequireQualifiedAccess>]
    module private TableName =
        let value = function
            | TableName.Item -> "Item"
            | TableName.Tag -> "Tag"
            | TableName.Product -> "Product"
            | TableName.Set -> "Set"
            | TableName.User -> "User"

    [<RequireQualifiedAccess>]
    module private TableAttribute =
        let [<Literal>] PartitionKey = "PartitionKey"
        let [<Literal>] RowKey = "RowKey"

    [<RequireQualifiedAccess>]
    type Owner =
        | User of Id

    [<RequireQualifiedAccess>]
    module private Owner =
        let value = function
            | Owner.User id -> id |> Id.value

    [<RequireQualifiedAccess>]
    type TableError =
        | TableError of TableName * string
        | ExecuteError of TableName * exn

    [<RequireQualifiedAccess>]
    module TableError =
        let tableError table error = TableError.TableError (table, error)
        let executeError table error = TableError.ExecuteError (table, error)

        let format = function
            | TableError.TableError (table, error) -> sprintf "Insert data to the table %A ends with error %A." (table |> TableName.value) error
            | TableError.ExecuteError (table, error) -> sprintf "Isert data to the table %A ends with error %A" (table |> TableName.value) error

    [<RequireQualifiedAccess>]
    type TagCategory =
        | UserDefined

    [<RequireQualifiedAccess>]
    module private TagCategory =
        let value = function
            | TagCategory.UserDefined -> "UserDefined"

    [<AutoOpen>]
    module private ParseHelpers =
        open Shared

        let parseMany separator parse =
            String.parse
            >> Option.map (
                String.split separator
                >> Seq.toList
                >> List.choose parse
            )
            >> Option.defaultValue []

        let parseLinks =
            (Link.parse >> Result.toOption) |> parseMany ' '

    //
    // Find sub-entities
    //

    type private FindProduct = FindProduct of (Manufacturer * Id -> AsyncResult<ProductInfo option, TableError>)

    //
    // Entities
    //

    type TagDbEntity(category: string, slug: string, tagName: string) =
        inherit TableEntity(
            partitionKey = category,
            rowKey = slug
        )

        new () = TagDbEntity(null, null, null)

        new (category, tag: Tag) =
            let category = category |> TagCategory.value
            let slug = tag.Slug |> Slug.value
            let name = tag.Name |> TagName.value

            TagDbEntity(category, slug, name)

        member val TagName = tagName with get, set

    [<RequireQualifiedAccess>]
    module private TagDbEntity =
        let hydrate: TagDbEntity -> Tag option = fun dbEntity -> maybe {
            let! slug = dbEntity.RowKey |> Slug.create

            return {
                Slug = slug
                Name = TagName dbEntity.TagName
            }
        }

    [<AutoOpen>]
    module UserEntity =
        open Shared

        type UserDbEntity(userType: string, id: string, username: string, password: string, email: string, slug: string) =
            inherit TableEntity(
                partitionKey = userType,
                rowKey = id
            )

            new () = UserDbEntity(null, null, null, null, null, null)

            new (userType, userProfile: UserProfile, password) =
                let userType = userType |> UserType.value
                let id = userProfile.Id |> Id.value
                let userName = userProfile.Username |> Username.value
                let password =
                    match password |> Password.encryptedValue with
                    | Some password -> password
                    | _ -> failwith "Password is not encrypted!"
                let email = userProfile.Email |> Email.value
                let slug = userProfile.Slug |> Slug.value

                UserDbEntity(userType, id, userName, password, email, slug)

            member val Username = username with get, set
            member val Password = password with get, set
            member val Email = email with get, set
            member val Slug = slug with get, set

        [<RequireQualifiedAccess>]
        module UserDbEntity =
            let hydrate password: UserDbEntity -> UserProfile option = fun dbEntity -> maybe {
                do! Password.verify (Current dbEntity.Password, TypedIn password)

                let! id = dbEntity.RowKey |> Id.tryParse
                let username = Username dbEntity.Username
                let email = Email dbEntity.Email
                let slug = Slug dbEntity.Slug

                return {
                    Id = id
                    Username = username
                    Email = email
                    Slug = slug
                }
            }

            let private filterByUsernameOrEmail usernameOrEmail =
                TableQuery.CombineFilters(
                    TableQuery.GenerateFilterCondition("Username", QueryComparisons.Equal, usernameOrEmail |> UsernameOrEmail.value),
                    TableOperators.Or,
                    TableQuery.GenerateFilterCondition("Email", QueryComparisons.Equal, usernameOrEmail |> UsernameOrEmail.value)
                )

            let private filterBySlug slug =
                TableQuery.GenerateFilterCondition("Slug", QueryComparisons.Equal, slug |> Slug.value)

            let queryUser userType usernameOrEmail =
                TableQuery<UserDbEntity>().Where(
                    TableQuery.CombineFilters(
                        TableQuery.GenerateFilterCondition(TableAttribute.PartitionKey, QueryComparisons.Equal, userType |> UserType.value),
                        TableOperators.And,
                        filterByUsernameOrEmail usernameOrEmail
                    )
                )

            let exists userType usernameOrEmail slug =
                TableQuery<UserDbEntity>().Where(
                    TableQuery.CombineFilters(
                        TableQuery.GenerateFilterCondition(TableAttribute.PartitionKey, QueryComparisons.Equal, userType |> UserType.value),
                        TableOperators.And,
                        TableQuery.CombineFilters(
                            filterByUsernameOrEmail usernameOrEmail,
                            TableOperators.Or,
                            filterBySlug slug
                        )
                    )
                )

    type ProductDbEntity(manufacturer: string, id: string, name: string, priceAmount: float, priceCurrency: string, ean: string, links: string) =
        inherit TableEntity(
            partitionKey = manufacturer,
            rowKey = id
        )

        new () = ProductDbEntity(null, null, null, 0.0, null, null, null)

        new (product: ProductInfo) =
            ProductDbEntity(
                product.Manufacturer |> Manufacturer.value,
                product.Id |> Id.value,
                product.Name,
                product.Price.Amount,
                product.Price.Currency |> Currency.value,
                product.Ean |> Option.map Ean.value |> Option.defaultValue null,
                product.Links |> List.map Link.value |> String.concat " "
            )

        member val Name = name with get, set
        member val PriceAmount = priceAmount with get, set
        member val PriceCurrency = priceCurrency with get, set
        member val Ean = ean with get, set
        member val Links = links with get, set

    [<RequireQualifiedAccess>]
    module private ProductDbEntity =
        open Shared
        open ErrorHandling.Option.Operators

        let hydrate: ProductDbEntity -> ProductInfo option = fun dbEntity -> maybe {
            let! id =
                dbEntity.RowKey
                |> Id.tryParse

            let! price =
                dbEntity.PriceCurrency
                |> Currency.parse
                <!> Price.create dbEntity.PriceAmount

            let ean =
                dbEntity.Ean
                |> String.parse
                <!> Ean

            return {
                Id = id
                Name = dbEntity.Name
                Manufacturer = Manufacturer dbEntity.PartitionKey
                Price = price
                Ean = ean
                Links = dbEntity.Links |> parseLinks
            }
        }

    type ItemDbEntity(owner: string, id: string,
        itemType: string, subType, name: string, note: string, color: string, tags: string, links: string, ownershipStatus: string, product: string,
        priceAmount: float, priceCurrency: string,
        weight: int, height: int, width: int, length: int,
        galleryImages: string,
        inventory: string) =

        inherit TableEntity(
            partitionKey = owner,
            rowKey = id
        )

        new () = ItemDbEntity(null, null, null, null, null, null, null, null, null, null, null, 0.0, null, 0, 0, 0, 0, null, null)

        new (owner, item: ItemEntity) =
            // todo - not all data are save this way, specific data for any type/subType are not there (this means not use a FlatItem here, but parse it directly with all consequences)
            let fItem = item |> FlatItem.ofItemEntity |> FlatItem.data

            let color =
                fItem.Common.Color
                |> Option.map Color.value
                |> Option.defaultValue null

            let product =
                fItem.Common.Product
                |> Option.map (fun p ->
                    sprintf "%s|%s" (p.Manufacturer |> Manufacturer.value) (p.Id |> Id.value)
                )
                |> Option.defaultValue null

            let priceAmmount, priceCurrency =
                fItem.Common.Price
                |> Option.map (fun p -> p.Amount, p.Currency |> Currency.value)
                |> Option.defaultValue (0.0, null)

            let weight, height, width, length =
                fItem.Common.Size
                |> Option.map (fun s ->
                    let weight =
                        s.Weight
                        |> Option.map Weight.value
                        |> Option.defaultValue 0

                    let height, width, length =
                        s.Dimensions
                        |> Option.map (fun d ->
                            d.Height |> int,
                            d.Width |> int,
                            d.Length |> int
                        )
                        |> Option.defaultValue (0, 0, 0)

                    weight, height, width, length
                )
                |> Option.defaultValue (0, 0, 0, 0)

            let galleryImages =
                fItem.Common.Gallery
                |> Option.map (fun g ->
                    g.Images |> List.map Link.value |> String.concat " "
                )
                |> Option.defaultValue null

            ItemDbEntity(
                owner,
                item.Id |> Id.value,
                fItem.Type,
                fItem.SubType |> Option.defaultValue "Other",
                fItem.Common.Name,
                fItem.Common.Note |> Option.defaultValue null,
                color |> String.nullable,
                fItem.Common.Tags |> List.map Tag.value |> String.concat ",",
                fItem.Common.Links |> List.map Link.value |> String.concat " ",
                fItem.Common.OwnershipStatus |> OwnershipStatus.value,
                product,
                priceAmmount,
                priceCurrency,
                weight,
                height,
                width,
                length,
                galleryImages,
                null // inventory, for containers
            )

        member val Type = itemType with get, set
        member val SubType = subType with get, set
        member val Name = name with get, set
        member val Note = note with get, set
        member val Color = color with get, set
        member val Tags = tags with get, set
        member val Links = links with get, set
        member val PriceAmount = priceAmount with get, set
        member val PriceCurrency = priceCurrency with get, set
        member val Weight = weight with get, set
        member val Height = height with get, set
        member val Width = width with get, set
        member val Length = length with get, set
        member val OwnershipStatus = ownershipStatus with get, set
        member val Product = product with get, set
        member val Inventory = inventory with get, set
        member val GalleryImages = galleryImages with get, set

    [<RequireQualifiedAccess>]
    module private ItemDbEntity =
        open Shared
        open ErrorHandling.Option.Operators

        let private (|IsZeroOrLess|_|) = function
            | lessThenZero when lessThenZero <= 0 -> Some IsZeroOrLess
            | _ -> None

        let private parseTags =
            Tag.parse |> parseMany ','

        let private parseInventory inventory: (ItemInContainer list) option =
            // todo
            None

        let hydrate (FindProduct findProduct): ItemDbEntity -> AsyncResult<ItemEntity, string> = fun dbEntity -> asyncResult {
            let! id =
                dbEntity.RowKey
                |> Id.tryParse
                |> AsyncResult.ofOption "Id"

            let! ownershipStatus =
                dbEntity.OwnershipStatus
                |> OwnershipStatus.parse
                |> AsyncResult.ofOption "OwnershipStatus"

            let price =
                dbEntity.PriceCurrency
                |> Currency.parse
                <!> Price.create dbEntity.PriceAmount

            let weight =
                match dbEntity.Weight with
                | IsZeroOrLess -> None
                | weight -> weight |> Weight.ofGrams |> Some

            let dimensions =
                match dbEntity.Height, dbEntity.Width, dbEntity.Length with
                | IsZeroOrLess, _, _
                | _, IsZeroOrLess, _
                | _, _, IsZeroOrLess -> None
                | height, width, length ->
                    Some {
                        Height = height |> Dimensions.ofMilimeter
                        Width = width |> Dimensions.ofMilimeter
                        Length = length |> Dimensions.ofMilimeter
                    }

            let size =
                match weight, dimensions with
                | None, None -> None
                | weight, dimensions ->
                    Some {
                        Weight = weight
                        Dimensions = dimensions
                    }

            let galleryImages =
                dbEntity.GalleryImages
                |> parseLinks

            let gallery =
                match galleryImages with
                | [] -> None
                | images -> Some {
                    Images = images
                }

            let productPoint = maybe {
                let! serializedProduct = dbEntity.Product |> String.parse

                return!
                    match serializedProduct.Split('|', 2) with
                    | [| manufacturer; productId |] ->
                        maybe {
                            let! productId = productId |> Id.tryParse
                            let! manufacturer = manufacturer |> Manufacturer.parse

                            return (manufacturer, productId)
                        }
                    | _ -> None
            }

            let! product =
                match productPoint with
                | Some productPoint ->
                    productPoint
                    |> findProduct
                    |> AsyncResult.mapError TableError.format
                | _ -> AsyncResult.ofSuccess None

            let common = {
                Name = dbEntity.Name
                Note = dbEntity.Note |> String.parse
                OwnershipStatus = ownershipStatus
                Color = dbEntity.Color |> String.parse >>= Color.parse
                Tags = dbEntity.Tags |> parseTags
                Links = dbEntity.Links |> parseLinks
                Price = price
                Size = size
                Product = product
                Gallery = gallery
            }

            let! item =
                match dbEntity.Type, dbEntity.SubType with
                // Tools
                | "Tool", "MultiTool" -> Item.Tool (MultiTool { Common = common }) |> Some
                | "Tool", "Knife" -> Item.Tool (Knife { Common = common }) |> Some
                | "Tool", "Gun" -> Item.Tool (Gun { Common = common }) |> Some
                | "Tool", _ -> Item.Tool (OtherTool { Common = common }) |> Some
                // Container
                | "Container", "BagPack" ->
                    maybe {
                        let! items = dbEntity.Inventory |> parseInventory
                        let totalSize = items |> Stats.sumInventorySize

                        return Item.Container (BagPack { Common = common; Items = items; TotalSize = totalSize })
                    }
                | "Container", "Organizer" ->
                    maybe {
                        let! items = dbEntity.Inventory |> parseInventory
                        let totalSize = items |> Stats.sumInventorySize

                        return Item.Container (Organizer { Common = common; Items = items; TotalSize = totalSize })
                    }
                | "Container", "Pocket" ->
                    maybe {
                        let! items = dbEntity.Inventory |> parseInventory
                        let totalSize = items |> Stats.sumInventorySize

                        return Item.Container (Pocket { Common = common; Items = items; TotalSize = totalSize })
                    }
                | "Container", "Panel" ->
                    maybe {
                        let! items = dbEntity.Inventory |> parseInventory
                        let totalSize = items |> Stats.sumInventorySize

                        return Item.Container (Panel { Common = common; Items = items; TotalSize = totalSize })
                    }
                | "Container", _ ->
                    maybe {
                        let! items = dbEntity.Inventory |> parseInventory
                        let totalSize = items |> Stats.sumInventorySize

                        return Item.Container (OtherContainer { Common = common; Items = items; TotalSize = totalSize })
                    }
                // Consumable
                | "Consumable", "Food" -> Item.Consumable (Food { Common = common }) |> Some
                | "Consumable", _ -> Item.Consumable (OtherConsumable { Common = common }) |> Some
                | _ -> None // todo - spis jako error, nebo aspon zalogovat ...
                |> AsyncResult.ofOption "Item"

            return {
                Id = id
                Item = item
            }
        }

    [<RequireQualifiedAccess>]
    module Table =
        open ErrorHandling.AsyncResult.Operators

        [<AutoOpen>]
        module private Common =
            // @see https://docs.microsoft.com/cs-cz/dotnet/fsharp/using-fsharp-on-azure/table-storage

            let table (storageAccount: CloudStorageAccount) tableName =
                try
                    let tableClient = storageAccount.CreateCloudTableClient()

                    tableClient.GetTableReference(tableName |> TableName.value)
                    |> AsyncResult.ofSuccess
                with
                | e -> TableError.TableError (tableName, e.Message) |> AsyncResult.ofError

            let private insert operation storageAccount tableName entity = asyncResult {
                let! table = tableName |> table storageAccount

                let! _ =
                    table.ExecuteAsync(operation entity)
                    |> AsyncResult.ofTaskCatch (TableError.executeError tableName)

                return ()
            }

            let insertEntity storageAccount tableName entity = insert TableOperation.Insert storageAccount tableName entity
            let insertOrReplaceEntity storageAccount tableName entity = insert TableOperation.InsertOrReplace storageAccount tableName entity

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

        let private queryPartition<'TElement> partitionKey =
            TableQuery<'TElement>().Where(
                TableQuery.GenerateFilterCondition(TableAttribute.PartitionKey, QueryComparisons.Equal, partitionKey)
            )

        let private queryPoint<'TElement> (partitionKey, rowKey) =
            TableQuery<'TElement>().Where(
                TableQuery.CombineFilters(
                    TableQuery.GenerateFilterCondition(TableAttribute.PartitionKey, QueryComparisons.Equal, partitionKey),
                    TableOperators.And,
                    TableQuery.GenerateFilterCondition(TableAttribute.RowKey, QueryComparisons.Equal, rowKey)
                )
            )

        let private fetch<'TElement when 'TElement : (new : unit -> 'TElement) and 'TElement :> ITableEntity> (table: CloudTable) (query: TableQuery<'TElement>) =
            table.ExecuteQuery(query)

        let private select (storageAccount: CloudStorageAccount) tableName query = asyncResult {
            let! table = tableName |> table storageAccount
            let! result =
                try query |> fetch table |> AsyncResult.ofSuccess
                with e -> TableError.ExecuteError (tableName, e) |> AsyncResult.ofError

            return result |> Seq.toList
        }

        let private selectEntities storageAccount tableName =
            queryPartition
            >> select storageAccount tableName

        let private selectEntity (storageAccount: CloudStorageAccount) tableName =
            queryPoint
            >> select storageAccount tableName
            >> AsyncResult.map List.tryHead

        // Tags

        let insertTag storageAccount category (tag: Tag) =
            TagDbEntity(category, tag)
            |> insertOrReplaceEntity storageAccount TableName.Tag

        let selectTags storageAccount category =
            category
            |> TagCategory.value
            |> selectEntities storageAccount TableName.Tag
            <!> List.choose TagDbEntity.hydrate

        // Product

        let insertProduct storageAccount (product: ProductInfo) =
            ProductDbEntity(product)
            |> insertEntity storageAccount TableName.Product

        let insertOrReplaceProduct storageAccount (product: ProductInfo) =
            ProductDbEntity(product)
            |> insertOrReplaceEntity storageAccount TableName.Product

        let selectProducts storageAccount manufacturer =
            manufacturer
            |> selectEntities storageAccount TableName.Product
            <!> List.choose ProductDbEntity.hydrate

        let private selectProduct storageAccount = FindProduct (fun (manufacturer, personId) ->
            (manufacturer |> Manufacturer.value, personId |> Id.value)
            |> selectEntity storageAccount TableName.Product
            <!> Option.bind ProductDbEntity.hydrate
        )

        // Items

        let insertItem storageAccount owner (item: ItemEntity) = asyncResult {
            let owner = owner |> Owner.value

            let { Tags = tags; Product = product } = item.Item |> Item.common
            let! _ =
                tags
                |> List.map (insertTag storageAccount TagCategory.UserDefined)
                |> AsyncResult.sequenceM // todo - create a batch insertOrIgnore

            match product with
            | Some product -> do! product |> insertOrReplaceProduct storageAccount
            | _ -> ()

            return!
                ItemDbEntity(owner, item)
                |> insertEntity storageAccount TableName.Item
        }

        let selectItems logError storageAccount owner = asyncResult {
            let! entities =
                owner
                |> Owner.value
                |> selectEntities storageAccount TableName.Item

            let! hydratedEntities =
                entities
                |> List.map (ItemDbEntity.hydrate (selectProduct storageAccount) >> (AsyncResult.teeError logError))
                |> Async.Parallel
                |> AsyncResult.ofAsyncCatch (TableError.executeError TableName.Item)

            return
                hydratedEntities
                |> Seq.toList
                |> Result.list
        }

        // Users

        let insertUser storageAccount userType password (userProfile: UserProfile) =
            try
                UserDbEntity(userType, userProfile, password)
                |> insertEntity storageAccount TableName.User
            with
            | e -> TableError.ExecuteError (TableName.User, e) |> AsyncResult.ofError

        let selectUser logError storageAccount userType usernameOrEmail password =
            usernameOrEmail
            |> UserDbEntity.queryUser userType
            |> select storageAccount TableName.User
            |> AsyncResult.teeError (TableError.format >> logError)
            |> AsyncResult.map (List.tryHead >> Option.bind (UserDbEntity.hydrate password))

        let isUserExists logError storageAccount userType usernameOrEmail slug =
            UserDbEntity.exists userType usernameOrEmail slug
            |> select storageAccount TableName.User
            |> AsyncResult.teeError (TableError.format >> logError)
            |> AsyncResult.map (List.tryHead >> Option.isSome)
