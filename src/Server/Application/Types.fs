namespace MF.EDC

type ConnectionString = ConnectionString of string
type DatabasePassword = DatabasePassword of string

type PublicPath = PublicPath of string

[<RequireQualifiedAccess>]
module PublicPath =
    let value (PublicPath path) = path

type AppInsightKey = AppInsightKey of string

[<RequireQualifiedAccess>]
module AppInsightKey =
    let value (AppInsightKey key) = key
