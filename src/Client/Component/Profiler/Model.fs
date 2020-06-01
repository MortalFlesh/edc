module ProfilerModel

open Elmish
open Shared

type ProfilerModel = {
    Profiler: Profiler.Toolbar option
}

[<RequireQualifiedAccess>]
type ProfilerAction =
    | ShowProfiler of Profiler.Toolbar option

type DispatchProfilerAction = ProfilerAction -> unit

[<RequireQualifiedAccess>]
module ProfilerModel =
    let empty = {
        Profiler = None
    }

    let update (model: ProfilerModel) = function
        | ProfilerAction.ShowProfiler profiler -> { model with Profiler = profiler }, Cmd.none
