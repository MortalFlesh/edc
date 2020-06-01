namespace MF.EDC.Profiler

module Errors =
    open System
    open Shared
    open State.ConcurrentStorage

    type Errors = private Errors of State<DateTime, ErrorMessage * DateTime>

    let mutable private errors = Errors (State.empty())
    let mutable private totalCount = 0

    [<RequireQualifiedAccess>]
    module Errors =
        let private state (Errors state) = state

        let add message =
            let now = DateTime.Now

            errors
            |> state
            |> State.set (Key now) (message, now)

            totalCount <- totalCount + 1
            errors <-
                errors
                |> state
                |> State.keepLastSortedBy 10 fst
                |> Errors

        let values () =
            errors
            |> state
            |> State.items
            |> List.sortByDescending fst
            |> List.map snd

        let count () = totalCount
