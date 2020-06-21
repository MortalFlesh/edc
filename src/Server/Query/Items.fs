namespace MF.EDC.Query

open ErrorHandling
open MF.EDC

[<RequireQualifiedAccess>]
type ItemsError =
    | Runtime of string
    | TableError of Database.CloudStorage.TableError

[<RequireQualifiedAccess>]
module ItemsError =
    let format = function
        | ItemsError.Runtime error -> error
        | ItemsError.TableError error -> error |> Database.CloudStorage.TableError.format

type LoadItemsQuery = Query<ItemEntity list, ItemsError>

[<RequireQualifiedAccess>]
module ItemsQuery =
    open ErrorHandling.AsyncResult.Operators

    module private CloudStorage =
        open Database.CloudStorage

        let loadUserItems logError storageAccount username =
            Table.selectItems logError storageAccount (Owner.User username) <@> ItemsError.TableError

    let load logError storageAccount username: LoadItemsQuery = asyncResult {
        let! items =
            username
            |> CloudStorage.loadUserItems logError storageAccount >>@ (ItemsError.format >> eprintfn "StorageTable load failed! %A")  // todo - log error

        return items
    }
