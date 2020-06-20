[<AutoOpen>]
module Types

open Shared

type AsyncStatus =
    | Inactive
    | InProgress
    | Completed

type InputState =
    | Neutral
    | Success
    | WithError of ErrorMessage

module FlatItem =
    open Dto.Common
    open Dto.Items

    //
    // Flatten types, to simplify a view
    //

    type FlatItemData<'Original> = {
        Common: CommonInfo
        Type: string
        SubType: string option
        Original: 'Original
    }

    type FlatItemEntity<'Original> = {
        Id: Id
        Item: FlatItemData<'Original>
    }

    [<RequireQualifiedAccess>]
    type FlatItem<'Original> =
        | Entity of FlatItemEntity<'Original>
        | Data of FlatItemData<'Original>

    [<RequireQualifiedAccess>]
    module FlatItem =
        let private mapOriginal original = function
            | FlatItem.Entity { Id = id; Item = item } ->
                FlatItem.Entity {
                    Id = id
                    Item = {
                        Common = item.Common
                        Type = item.Type
                        SubType = item.SubType
                        Original = original
                    }
                }
            | FlatItem.Data item ->
                FlatItem.Data {
                    Common = item.Common
                    Type = item.Type
                    SubType = item.SubType
                    Original = original
                }

        let data = function
            | FlatItem.Entity { Item = item }
            | FlatItem.Data item -> item

        let entity = function
            | FlatItem.Entity entity -> Some entity
            | _ -> None

        let ofTool = FlatItem.Data << function
            | MultiTool { Common = common } as original ->
                {
                    Common = common
                    Type = "Tool"
                    SubType = Some "MultiTool"
                    Original = original
                }
            | Knife { Common = common } as original ->
                {
                    Common = common
                    Type = "Tool"
                    SubType = Some "Knife"
                    Original = original
                }
            | Gun { Common = common } as original ->
                {
                    Common = common
                    Type = "Tool"
                    SubType = Some "Gun"
                    Original = original
                }
            | Tool.OtherTool { Common = common } as original ->
                {
                    Common = common
                    Type = "Tool"
                    SubType = None
                    Original = original
                }

        let ofContainer = FlatItem.Data << function
            | BagPack bagPack as original ->
                {
                    Common = bagPack.Common
                    Type = "Container"
                    SubType = Some "BagPack"
                    Original = original
                }
            | Organizer organizer as original ->
                {
                    Common = organizer.Common
                    Type = "Container"
                    SubType = Some "Organizer"
                    Original = original
                }
            | Pocket pocket as original ->
                {
                    Common = pocket.Common
                    Type = "Container"
                    SubType = Some "Pocket"
                    Original = original
                }
            | Panel panel as original ->
                {
                    Common = panel.Common
                    Type = "Container"
                    SubType = Some "Panel"
                    Original = original
                }
            | OtherContainer other as original ->
                {
                    Common = other.Common
                    Type = "Container"
                    SubType = None
                    Original = original
                }

        let ofConsumable = FlatItem.Data << function
            | Food food as original ->
                {
                    Common = food.Common
                    Type = "Consumable"
                    SubType = Some "Food"
                    Original = original
                }
            | Consumable.OtherConsumable other as original ->
                {
                    Common = other.Common
                    Type = "Consumable"
                    SubType = None
                    Original = original
                }

        let ofItem = function
            | Item.Tool tool as original -> tool |> ofTool |> mapOriginal original
            | Item.Container container as original -> container |> ofContainer |> mapOriginal original
            | Item.Consumable consumable as original -> consumable |> ofConsumable |> mapOriginal original

        let ofItemEntity ({ Id = id; Item = item }: ItemEntity) =
            FlatItem.Entity {
                Id = id;
                Item = item |> ofItem |> data
            }

    [<RequireQualifiedAccess>]
    module FlatItemEntity =
        let ofItemEntity ({ Id = id; Item = item }: ItemEntity) =
            {
                Id = id;
                Item = item |> FlatItem.ofItem |> FlatItem.data
            }
