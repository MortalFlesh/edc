module PageEdcModule

open Elmish

open Shared
open Shared.Dto.Common
open Shared.Dto.Edc

type PageEdcModel = {
    SelectedSet: Id option

    Sets: Id list
    SetsLoadingStatus: AsyncStatus
}

//
// Messages / Actions
//

type SetsAction =
    | RefreshSets
    | SetsLoaded of Id list
    | SetsLoadedWithError of ErrorMessage
    | HideSetsLoadingStatus

[<RequireQualifiedAccess>]
type PageEdcAction =
    | InitPage of Id option
    | SelectSet of Id option
    | SetsAction of SetsAction

type DispatchPageEdcAction = PageEdcAction -> unit

[<RequireQualifiedAccess>]
module PageEdcModel =
    let empty = {
        SelectedSet = None

        Sets = []
        SetsLoadingStatus = Inactive
    }

    let update<'GlobalAction>
        (liftAction: PageEdcAction -> 'GlobalAction)
        (showSuccess: SuccessMessage -> 'GlobalAction)
        (showError: ErrorMessage -> 'GlobalAction)
        (authError: ErrorMessage -> 'GlobalAction)
        (model: PageEdcModel) = function

        | PageEdcAction.InitPage None ->
            { empty with Sets = model.Sets }, Cmd.batch [
                Cmd.ofMsg (PageEdcAction.SetsAction RefreshSets)
            ]
            |> Cmd.map liftAction
        | PageEdcAction.InitPage set ->
            { empty with Sets = model.Sets }, Cmd.batch [
                Cmd.ofMsg (PageEdcAction.SetsAction RefreshSets)
                Cmd.ofMsg (PageEdcAction.SelectSet set)
            ]
            |> Cmd.map liftAction

        // Select service
        | PageEdcAction.SelectSet None -> { model with SelectedSet = None }, Cmd.none
        | PageEdcAction.SelectSet (Some set) -> { model with SelectedSet = Some set }, Cmd.none

        // Sets
        | PageEdcAction.SetsAction RefreshSets ->
            { model with SetsLoadingStatus = InProgress },
            Cmd.none
            (* Cmd.OfAsyncImmediate.result (
                Api.loadSets
                    (SetsLoaded >> SetsMessage >> liftAction)
                    (SetsLoadedWithError >> SetsMessage >> liftAction)
                    authError
                    ()
            ) *)
        | PageEdcAction.SetsAction (SetsLoaded sets) ->
            { model with Sets = sets; SetsLoadingStatus = Completed },
            Cmd.OfAsyncImmediate.result (async {
                do! Async.Sleep (2 * 1000)
                return PageEdcAction.SetsAction HideSetsLoadingStatus
            })
            |> Cmd.map liftAction
        | PageEdcAction.SetsAction (SetsLoadedWithError error) ->
            { model with Sets = [] },
            Cmd.batch [
                Cmd.ofMsg (showError error)
                Cmd.ofMsg (PageEdcAction.SetsAction HideSetsLoadingStatus) |> Cmd.map liftAction
            ]
        | PageEdcAction.SetsAction HideSetsLoadingStatus -> { model with SetsLoadingStatus = Inactive }, Cmd.none
