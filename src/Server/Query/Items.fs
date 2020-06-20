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
    let private staticItems = [
        {
            Id = Id.create()
            Item = Item.Tool (Knife {
                Common = {
                    Name = "Gerber Metolius Fixed"
                    Note = None; Color = None; Tags = []; Links = []; Price = None
                    Size = Some {
                        Weight = Some (Weight 170<Gram>)
                        Dimensions = Some { Height = 40<Milimeter>; Width = 25<Milimeter>; Length = 220<Milimeter> }
                    }
                    OwnershipStatus = Own
                    Product = Some {
                        Id = Id.create()
                        Manufacturer = Manufacturer "Gerber"
                        Name = "Gerber Motelius"
                        Price = { Amount = 1200.; Currency = Czk }
                        Ean = None
                        Links = []
                    }
                    Gallery = None
                }
            })
        }

        {
            Id = Id.create()
            Item = Item.Tool (Knife {
                Common = {
                    Name = "Gerber Metolius Foldable"
                    Note = None; Color = None; Tags = []; Size = None
                    Links = [
                        Link "http://www.noze-nuz.com/gerber/G0009.php"
                    ]
                    Price = Some { Amount = 1200.; Currency = Czk }
                    OwnershipStatus = Wish
                    Product = Some {
                        Id = Id.create()
                        Manufacturer = Manufacturer "Gerber"
                        Name = "Gerber Metolius Foldable"
                        Price = {
                            Amount = 1200.
                            Currency = Czk
                        }
                        Ean = None
                        Links = [
                            Link "https://moskito.cz/produkt/zaviraci-nuz-gerber-metolius-folder/"
                            Link "https://matum.cz/zaviraci-nuz-metolius-14298"
                        ]
                    }
                    Gallery = None
                }
            })
        }
    ]

    open ErrorHandling.AsyncResult.Operators

    module private MySql =
        open Database.MySql

        let load connection =
            MySql.select connection () <@> ItemsError.Runtime

    module private AzureSql =
        open Database.AzureSql

        let load connection =
            AzureSql.select connection () <@> ItemsError.Runtime

    module private CloudStorage =
        open Database.CloudStorage

        let loadUserItems logError storageAccount username =
            Table.selectItems logError storageAccount (Owner.User username) <@> ItemsError.TableError

    let load logError storageAccount mysqlConnection azureSqlConnection username: LoadItemsQuery = asyncResult {
        let! items = CloudStorage.loadUserItems logError storageAccount username >>@ (ItemsError.format >> eprintfn "StorageTable load failed! %A")  // todo - log error

        (* let! results =
            [
                CloudStorage.load storageAccount >>* (fun _ -> printfn "StorageTable load done!") >>@ (ItemsError.format >> eprintfn "StorageTable load failed! %A")
                // connection refused ?

                //MySql.load mysqlConnection >>* (fun _ -> printfn "MySql load done!") >>@ (ItemsError.format >> eprintfn "MySql load failed! %A")
                // connection, not found server

                //AzureSql.load azureSqlConnection >>* (fun _ -> printfn "AzureSql load done!") >>@ (ItemsError.format >> eprintfn "AzureSql load failed! %A")
                // Invalid obj "Items"

                // todo - all gets connection refused...
            ]
            |> Async.Parallel
            |> AsyncResult.ofAsyncCatch (fun e -> e.Message |> ItemsError.Runtime) *)

        (* let! items =
            results
            |> Seq.toList
            |> Result.sequenceConcat
            |> AsyncResult.ofResult *)
            //|> Result.listCollect

        //let items = staticItems

        return items
    }
