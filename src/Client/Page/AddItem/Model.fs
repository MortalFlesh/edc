module PageAddItemModule

open Elmish

open Shared
open Shared.Dto.Common
open Shared.Dto.Items

type Field =
    | Name
    | Note

[<RequireQualifiedAccess>]
module Field =
    let value = function
        | Name -> "Name"
        | Note -> "Note"

type PageAddItemModel = {
    CommonInfo: CommonInfo
    Errors: Map<Field, ErrorMessage>
    SavingStatus: AsyncStatus
}

//
// Messages / Actions
//

[<RequireQualifiedAccess>]
type PageAddItemAction =
    | InitPage

    | ChangeName of string
    | ChangeNote of string

    | SaveItem
    | ItemSaved
    | ItemSavedWithError of ErrorMessage

type DispatchPageAddItemAction = PageAddItemAction -> unit

[<RequireQualifiedAccess>]
module PageAddItemModel =
    let empty = {
        CommonInfo = {
            Name = ""
            Note = None
            Color = None
            Tags = []
            Links = []
            Price = None
            Size = None
            OwnershipStatus = Idea
            Product = None
            Gallery = None
        }
        Errors = Map.empty
        SavingStatus = Inactive
    }

    let clearError field model =
        { model with Errors = model.Errors.Remove field }

    let update<'GlobalAction>
        (liftAction: PageAddItemAction -> 'GlobalAction)
        (showSuccess: SuccessMessage -> 'GlobalAction)
        (showError: ErrorMessage -> 'GlobalAction)
        (authError: ErrorMessage -> 'GlobalAction)
        (model: PageAddItemModel)
        : PageAddItemAction -> PageAddItemModel * Cmd<'GlobalAction> = function

        | PageAddItemAction.InitPage ->
            empty, Cmd.none

        | PageAddItemAction.ChangeName name ->
            { model with CommonInfo = { model.CommonInfo with Name = name }} |> clearError Name, Cmd.none

        | PageAddItemAction.ChangeNote note ->
            { model with CommonInfo = { model.CommonInfo with Note = note |> String.parse }} |> clearError Note, Cmd.none

        | PageAddItemAction.SaveItem ->
            // todo - validate (Name?)
            let model =
                match model.CommonInfo.Name |> String.parse with
                | Some _ -> { model with SavingStatus = InProgress }
                | _ -> { model with Errors = (Name, (ErrorMessage "Item must have a name.")) |> model.Errors.Add }

            model, Cmd.none

        | PageAddItemAction.ItemSaved ->
            model, Cmd.none

        | PageAddItemAction.ItemSavedWithError ->
            model, Cmd.none
