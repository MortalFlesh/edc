namespace MF.EDC.Database

open MF.EDC

module MySql =

    type ItemDbEntity = {
        Id: string
        Name: string
        // todo - add more cols
    }

    [<RequireQualifiedAccess>]
    module private ItemDbEntity =
        let hydrate: ItemDbEntity -> ItemEntity option = fun dbEntity -> maybe {
            let! id =
                dbEntity.Id
                |> Id.tryParse

            return {
                Id = id
                Item = Item.Tool (Knife {
                    Common = {
                        Name = dbEntity.Name
                        Note = Some "MySql"
                        OwnershipStatus = Own
                        Color = None; Tags = []; Links = []; Price = None; Size = None ; Product = None; Gallery = None
                    }
                })
            }
        }

    [<RequireQualifiedAccess>]
    module MySql =
        open System.Data
        open System.Data.SqlClient
        open Dapper.FSharp
        open Dapper.FSharp.MySQL
        open ErrorHandling

        let private connectDb (ConnectionString connectionString) =
            new SqlConnection(connectionString) :> IDbConnection

        let connect connectionString =
            OptionTypes.register()
            connectDb connectionString

        let insert (connection: IDbConnection) (itemEntity: ItemDbEntity) =
            insert {
                table "Items"
                value itemEntity
            }
            |> connection.InsertAsync
            |> AsyncResult.ofTaskCatch (fun e -> e.Message)
            |> AsyncResult.map ignore

        let select (connection: IDbConnection) () =
            select {
                table "Items"
            }
            |> connection.SelectAsync<ItemDbEntity>
            |> AsyncResult.ofTaskCatch (fun e -> e.Message)
            |> AsyncResult.map (Seq.toList >> List.choose ItemDbEntity.hydrate)
