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
    | Other of ToolInfo

//
// Consumables
//

type ConsumableInfo = {
    Common: CommonInfo
}

type Consumable =
    | Food of ConsumableInfo
    | Other of ConsumableInfo

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
    | Other of ContainerInfo

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
        | Tool.Other { Common = common } -> common

[<RequireQualifiedAccess>]
module Container =
    let common = function
        | BagPack { Common = common }
        | Organizer { Common = common }
        | Pocket { Common = common }
        | Panel { Common = common }
        | Other { Common = common } -> common

[<RequireQualifiedAccess>]
module Consumable =
    let common = function
        | Consumable.Food { Common = common }
        | Consumable.Other { Common = common } -> common

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
