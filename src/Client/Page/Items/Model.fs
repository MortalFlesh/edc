module PageItemsModule

open Elmish

open Shared
open Shared.Dto.Common
open Shared.Dto.Items
open Shared.Dto.Edc

type PageItemsModel = {
    ItemDetail: Id option

    Items: ItemEntity list
    ItemsLoadingStatus: AsyncStatus
}

//
// Messages / Actions
//

type ItemsAction =
    | LoadItems
    | ItemsLoaded of ItemEntity list
    | ItemsLoadedWithError of ErrorMessage

[<RequireQualifiedAccess>]
type PageItemsAction =
    | InitPage of Id option
    | ShowDetail of Id
    | HideDetail
    | ItemsAction of ItemsAction

type DispatchPageItemsAction = PageItemsAction -> unit

[<RequireQualifiedAccess>]
module PageItemsModel =
    let empty = {
        ItemDetail = None

        Items = []
        ItemsLoadingStatus = Inactive
    }

    let update<'GlobalAction>
        (liftAction: PageItemsAction -> 'GlobalAction)
        (showSuccess: SuccessMessage -> 'GlobalAction)
        (showError: ErrorMessage -> 'GlobalAction)
        (authError: ErrorMessage -> 'GlobalAction)
        (model: PageItemsModel) = function

        | PageItemsAction.InitPage item ->
            { empty with Items = model.Items }, Cmd.batch [
                Cmd.ofMsg (PageItemsAction.ItemsAction LoadItems)

                match item with
                | Some item -> Cmd.ofMsg (PageItemsAction.ShowDetail item)
                | _ -> ()
            ]
            |> Cmd.map liftAction

        // Select service
        | PageItemsAction.ShowDetail id -> { model with ItemDetail = Some id }, Cmd.none
        | PageItemsAction.HideDetail -> { model with ItemDetail = None }, Cmd.none

        // Items
        | PageItemsAction.ItemsAction LoadItems ->
            { model with ItemsLoadingStatus = InProgress },
            Cmd.OfAsyncImmediate.result (
                Api.loadItems
                    (ItemsLoaded >> PageItemsAction.ItemsAction >> liftAction)
                    (ItemsLoadedWithError >> PageItemsAction.ItemsAction >> liftAction)
                    authError
                    ()
            )
        | PageItemsAction.ItemsAction (ItemsLoaded items) ->
            { model with Items = items; ItemsLoadingStatus = Inactive }, Cmd.none
        | PageItemsAction.ItemsAction (ItemsLoadedWithError error) ->
            { model with Items = [] }, Cmd.ofMsg (showError error)
