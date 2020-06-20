namespace MF.EDC

//
// Tools
//

type ToolInfo = {
    Common: CommonInfo
}

type Tool =
    | MultiTool of ToolInfo
    | Knife of (* Fixed|Foldable of FoldedSize *) ToolInfo
    | Gun of ToolInfo
    | OtherTool of ToolInfo

//
// Consumables
//

type ConsumableInfo = {
    Common: CommonInfo
}

type Consumable =
    | Food of ConsumableInfo
    | OtherConsumable of ConsumableInfo

//
// Items
//

[<RequireQualifiedAccess>]
type Item =
    | Tool of Tool
    | Container of Container
    | Consumable of Consumable

//
// Containers
//

and Container =
    | BagPack of ContainerInfo
    | Organizer of ContainerInfo
    | Pocket of ContainerInfo
    | Panel of ContainerInfo
    | OtherContainer of ContainerInfo

and ContainerInfo = {
    Common: CommonInfo

    Items: ItemInContainer list
    TotalSize: Size
}

and ItemInContainer = {
    Item: ItemEntity
    Quantity: int
}

//
// Entities
//

and ItemEntity = {
    Id: Id
    Item: Item
}

type ContainerEntity = {
    Id: Id
    Container: Container
}

//
// Modules
//

[<RequireQualifiedAccess>]
module Tool =
    let common = function
        | Tool.MultiTool { Common = common }
        | Tool.Knife { Common = common }
        | Tool.Gun { Common = common }
        | Tool.OtherTool { Common = common } -> common

[<RequireQualifiedAccess>]
module Container =
    let common = function
        | BagPack { Common = common }
        | Organizer { Common = common }
        | Pocket { Common = common }
        | Panel { Common = common }
        | OtherContainer { Common = common } -> common

[<RequireQualifiedAccess>]
module Consumable =
    let common = function
        | Consumable.Food { Common = common }
        | Consumable.OtherConsumable { Common = common } -> common

[<RequireQualifiedAccess>]
module Item =
    let common = function
        | Item.Tool tool -> tool |> Tool.common
        | Item.Container container -> container |> Container.common
        | Item.Consumable consumable -> consumable |> Consumable.common

[<RequireQualifiedAccess>]
module ItemEntity =
    let item ({ Item = item }: ItemEntity) = item
    let common = item >> Item.common
    let name = common >> CommonInfo.name

module FlatItem =
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
                    Type = "Tool"   // todo - use constants from Item module (same in StorageTable)
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

[<RequireQualifiedAccess>]
module Stats =
    let sumInventorySize: ItemInContainer list -> Size = function
        | [] -> { Weight = None; Dimensions = None }
        | items ->
            let totalWeight =
                items
                |> List.sumBy (fun { Item = { Item = item }; Quantity = q } ->
                    match item |> Item.common with
                    | { Size = Some { Weight = Some weight } } -> weight |> Weight.value
                    | _ -> 0
                )
                |> Weight.ofGrams

            {
                Weight = Some totalWeight
                Dimensions = None
            }
