namespace MF.EDC

type ConnectionString = ConnectionString of string
type DatabasePassword = DatabasePassword of string

type MySqlConnectionString = MySqlConnectionString of ConnectionString
type AzureSqlConnectionString = AzureSqlConnectionString of ConnectionString

[<RequireQualifiedAccess>]
module MySqlConnectionString =
    let connectionString (MySqlConnectionString (ConnectionString string)) = string

[<RequireQualifiedAccess>]
module AzureSqlConnectionString =
    let connectionString (AzureSqlConnectionString (ConnectionString string)) = string

type PublicPath = PublicPath of string

[<RequireQualifiedAccess>]
module PublicPath =
    let value (PublicPath path) = path

type AppInsightKey = AppInsightKey of string

[<RequireQualifiedAccess>]
module AppInsightKey =
    let value (AppInsightKey key) = key
