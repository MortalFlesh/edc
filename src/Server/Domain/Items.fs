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
