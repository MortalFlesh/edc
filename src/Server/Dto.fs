namespace MF.EDC

[<RequireQualifiedAccess>]
module Dto =
    open Shared
    open ErrorHandling

    [<RequireQualifiedAccess>]
    module Serialize =
        [<AutoOpen>]
        module private Helper =
            open Dto.Common

            let weightOfGrams = Weight.ofGrams
            let toMilimeter: int<MF.EDC.Milimeter> -> int<Dto.Common.Milimeter> = int >> Dimensions.ofMilimeter

        let user: User -> Dto.Login.User =
            fun { Username = username; Token = token } -> {
                Username = username
                Token = token
            }

        let private ownership: OwnershipStatus -> Dto.Common.OwnershipStatus = function
            | Own -> Dto.Common.Own
            | Wish -> Dto.Common.Wish
            | Maybe -> Dto.Common.Maybe
            | Idea -> Dto.Common.Idea
            | ToBuy -> Dto.Common.ToBuy
            | ToSell -> Dto.Common.ToSell
            | Ordered -> Dto.Common.Ordered

        let private weight: Weight -> Dto.Common.Weight =
            Weight.value >> weightOfGrams

        let private dimensions: Dimensions -> Dto.Common.Dimensions =
            fun dimensions -> {
                Height = dimensions.Height |> toMilimeter
                Width = dimensions.Width |> toMilimeter
                Length = dimensions.Length |> toMilimeter
            }

        let private size: Size -> Dto.Common.Size =
            fun { Weight = w; Dimensions = d } -> {
                Weight = w |> Option.map weight
                Dimensions = d |> Option.map dimensions
            }

        let private color: Color -> Dto.Common.Color =
            Color.value >> Dto.Common.Color

        let private tag: Tag -> Dto.Common.Tag =
            Tag.value >> Dto.Common.Tag

        let private link: Link -> Dto.Common.Link =
            Link.value >> Dto.Common.Link

        let private ean: Ean -> Dto.Common.Ean =
            Ean.value >> Dto.Common.Ean

        let private price: Price -> Dto.Common.Price =
            fun { Amount = amount; Currency = currency } -> {
                Amount = amount
                Currency = currency |> Currency.value
            }

        let private productInfo: ProductInfo -> Dto.Common.ProductInfo =
            fun product -> {
                Name = product.Name
                Price = product.Price |> price
                Ean = product.Ean |> Option.map ean
                Links = product.Links |> List.map link
            }

        let private gallery: Gallery -> Dto.Common.Gallery =
            fun gallery -> {
                Images = gallery.Images |> List.map link
            }

        let private commonInfo: CommonInfo -> Dto.Common.CommonInfo =
            fun info -> {
                Name = info.Name
                Note = info.Note
                Color = info.Color |> Option.map color
                Tags = info.Tags |> List.map tag
                Links = info.Links |> List.map link
                Price = info.Price |> Option.map price
                Size = info.Size |> Option.map size
                OwnershipStatus = info.OwnershipStatus |> ownership
                Product = info.Product |> Option.map productInfo
                Gallery = info.Gallery |> Option.map gallery
            }

        let private toolInfo: ToolInfo -> Dto.Items.ToolInfo =
            fun info -> {
                Common = info.Common |> commonInfo
            }

        let private tool: Tool -> Dto.Items.Tool = function
            | MultiTool multiTool -> Dto.Items.MultiTool (multiTool |> toolInfo)
            | Knife knife -> Dto.Items.Knife (knife |> toolInfo)
            | Gun gun -> Dto.Items.Gun (gun |> toolInfo)
            | Tool.Other other -> Dto.Items.Tool.Other (other |> toolInfo)

        let private containerInfo itemInContainer: ContainerInfo -> Dto.Items.ContainerInfo =
            fun info -> {
                Common = info.Common |> commonInfo

                Items = info.Items |> List.map itemInContainer
                TotalSize = info.TotalSize |> size
            }

        let private consumableInfo: ConsumableInfo -> Dto.Items.ConsumableInfo =
            fun info -> {
                Common = info.Common |> commonInfo
            }

        let private consumable: Consumable -> Dto.Items.Consumable = function
            | Food food -> Dto.Items.Food (food |> consumableInfo)
            | Consumable.Other other -> Dto.Items.Consumable.Other (other |> consumableInfo)

        let rec item: Item -> Dto.Items.Item = function
            | Item.Tool t -> Dto.Items.Item.Tool (t |> tool)
            | Item.Container c -> Dto.Items.Item.Container (c |> container)
            | Item.Consumable c -> Dto.Items.Item.Consumable (c |> consumable)

        and itemEntity: ItemEntity -> Dto.Items.ItemEntity =
            fun { Id = id; Item = i } -> {
                Id = id |> Id.value |> Dto.Common.Id
                Item = i |> item
            }

        and private itemInContainer: ItemInContainer -> Dto.Items.ItemInContainer =
            fun { Item = i; Quantity = q } -> {
                Item = i |> itemEntity
                Quantity = q
            }

        and private container: Container -> Dto.Items.Container = function
            | BagPack bagPack -> Dto.Items.BagPack (bagPack |> containerInfo itemInContainer)
            | Organizer organizer -> Dto.Items.Organizer (organizer |> containerInfo itemInContainer)
            | Pocket pocket -> Dto.Items.Pocket (pocket |> containerInfo itemInContainer)
            | Panel panel -> Dto.Items.Panel (panel |> containerInfo itemInContainer)
            | Other other -> Dto.Items.Container.Other (other |> containerInfo itemInContainer)

    [<RequireQualifiedAccess>]
    module Deserialize =
        open ErrorHandling.Result.Operators

        let credentials: Username * Password -> Result<Credentials, CredentialsError> = function
            | Username "", Password "" -> Error EmptyCredentials
            | Username "", _ -> Error EmptyUsername
            | _, Password "" -> Error EmptyPassword
            | username, password -> Ok { Username = username; Password = password }
