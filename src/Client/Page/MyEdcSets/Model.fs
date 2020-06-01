module PageMyEdcModule

open Elmish

open Shared
open Shared.Dto.Edc

type PageMyEdcModel = {
    SelectedSet: EdcSet option

    Sets: EdcSet list
    SetsLoadingStatus: AsyncStatus
}

//
// Messages / Actions
//

type SetsAction =
    | RefreshSets
    | SetsLoaded of EdcSet list
    | SetsLoadedWithError of ErrorMessage
    | HideSetsLoadingStatus

type PageMyEdcAction =
    | InitPage of EdcSet option
    | SelectSet of EdcSet option
    | SetsAction of SetsAction

type DispatchPageMyEdcAction = PageMyEdcAction -> unit

[<RequireQualifiedAccess>]
module PageMyEdcModel =
    let empty = {
        SelectedSet = None

        Sets = []
        SetsLoadingStatus = Inactive
    }

    let update<'GlobalAction>
        (liftAction: PageMyEdcAction -> 'GlobalAction)
        (showSuccess: SuccessMessage -> 'GlobalAction)
        (showError: ErrorMessage -> 'GlobalAction)
        (authError: ErrorMessage -> 'GlobalAction)
        (model: PageMyEdcModel) = function

        | InitPage None ->
            { empty with Sets = model.Sets }, Cmd.batch [
                Cmd.ofMsg (SetsAction RefreshSets)
            ]
            |> Cmd.map liftAction
        | InitPage set ->
            { empty with Sets = model.Sets }, Cmd.batch [
                Cmd.ofMsg (SetsAction RefreshSets)
                Cmd.ofMsg (SelectSet set)
            ]
            |> Cmd.map liftAction

        // Select service
        | SelectSet None -> { model with SelectedSet = None }, Cmd.none
        | SelectSet (Some set) -> { model with SelectedSet = Some set }, Cmd.none

        // Sets
        | SetsAction RefreshSets ->
            { model with SetsLoadingStatus = InProgress },
            Cmd.none
            (* Cmd.OfAsyncImmediate.result (
                Api.loadSets
                    (SetsLoaded >> SetsMessage >> liftAction)
                    (SetsLoadedWithError >> SetsMessage >> liftAction)
                    authError
                    ()
            ) *)
        | SetsAction (SetsLoaded sets) ->
            { model with Sets = sets; SetsLoadingStatus = Completed },
            Cmd.OfAsyncImmediate.result (async {
                do! Async.Sleep (2 * 1000)
                return SetsAction HideSetsLoadingStatus
            })
            |> Cmd.map liftAction
        | SetsAction (SetsLoadedWithError error) ->
            { model with Sets = [] },
            Cmd.batch [
                Cmd.ofMsg (showError error)
                Cmd.ofMsg (SetsAction HideSetsLoadingStatus) |> Cmd.map liftAction
            ]
        | SetsAction HideSetsLoadingStatus -> { model with SetsLoadingStatus = Inactive }, Cmd.none
