namespace MF.EDC.Profiler

module Queries =
    open System
    open State.ConcurrentStorage

    type Url = Url of string
    type HTTPMethod =
        | Get
        | Post
        | Put
        | Delete

    type Target = Target of HTTPMethod * Url
    type Response = Response of Result<string, string>

    type TargetData = {
        Target: Target
        Created: DateTime
    }

    type QueryData = {
        Target: TargetData
        Response: string
    }

    type Query = Query of Result<QueryData, QueryData>

    [<RequireQualifiedAccess>]
    module Target =
        let value (Target (method, Url url)) = sprintf "[%A] %s" method url

    [<RequireQualifiedAccess>]
    module Response =
        let create response = Response response

    [<RequireQualifiedAccess>]
    module Query =
        let ofResponse targetData = function
            | Response (Ok response) -> Query (Ok { Target = targetData; Response = response })
            | Response (Error response) -> Query (Error { Target = targetData; Response = response })

    type Queries = private Queries of State<DateTime * Target, Query>

    let mutable private queries = Queries (State.empty())
    let mutable private totalCount = 0

    [<RequireQualifiedAccess>]
    module Queries =
        let private state (Queries state) = state

        let add target response =
            let now = DateTime.Now
            let data = {
                Target = target
                Created = now
            }

            queries
            |> state
            |> State.set
                (Key (now, target))
                (Query.ofResponse data response)

            totalCount <- totalCount + 1
            queries <-
                queries
                |> state
                |> State.keepLastSortedBy 10 fst
                |> Queries

        let values () =
            queries
            |> state
            |> State.items
            |> List.sortByDescending fst
            |> List.map snd

        let count () = totalCount
