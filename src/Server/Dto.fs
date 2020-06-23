namespace MF.EDC

type DeserializeItemError =
    | FOo

[<RequireQualifiedAccess>]
module DeserializeItemError =
    let format = function
        | DeserializeItemError.FOo -> ""

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

        let private size: Size -> Dto.Common.Weight option * Dto.Common.Dimensions option =
            fun { Weight = w; Dimensions = d } ->
                w |> Option.map weight,
                d |> Option.map dimensions

        let private color: Color -> Dto.Common.Color =
            Color.value >> Dto.Common.Color

        let tag: Tag -> Dto.Common.Tag = fun { Slug = Slug slug; Name = TagName tag } ->
            {
                Slug = Dto.Common.Slug slug
                Name = Dto.Common.TagName tag
            }

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
                Id = product.Id |> Id.value
                Name = product.Name
                Manufacturer = product.Manufacturer |> Manufacturer.value
                Price = product.Price |> price
                Ean = product.Ean |> Option.map ean
                Links = product.Links |> List.map link
            }

        let private gallery: Gallery -> Dto.Common.Gallery =
            fun gallery -> {
                Images = gallery.Images |> List.map link
            }

        let private commonInfo: CommonInfo -> Dto.Common.CommonInfo = fun info ->
            let weight, dimensions = info.Size |> Option.map size |> Option.defaultValue (None, None)

            {
                Name = info.Name
                Note = info.Note
                Color = info.Color |> Option.map color
                Tags = info.Tags |> List.map tag
                Links = info.Links |> List.map link
                Price = info.Price |> Option.map price
                Weight = weight
                Dimensions = dimensions
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
            | OtherTool other -> Dto.Items.OtherTool (other |> toolInfo)

        let private containerInfo itemInContainer: ContainerInfo -> Dto.Items.ContainerInfo =
            fun info -> {
                Common = info.Common |> commonInfo

                Items = info.Items |> List.map itemInContainer
                TotalWeight = info.TotalSize |> size |> fst |> Option.defaultValue (weightOfGrams 0)
            }

        let private consumableInfo: ConsumableInfo -> Dto.Items.ConsumableInfo =
            fun info -> {
                Common = info.Common |> commonInfo
            }

        let private consumable: Consumable -> Dto.Items.Consumable = function
            | Food food -> Dto.Items.Food (food |> consumableInfo)
            | OtherConsumable other -> Dto.Items.OtherConsumable (other |> consumableInfo)

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
            | OtherContainer other -> Dto.Items.Container.OtherContainer (other |> containerInfo itemInContainer)

    [<RequireQualifiedAccess>]
    module Deserialize =
        open ErrorHandling.Result.Operators

        [<AutoOpen>]
        module private Helper =
            open Dto.Common

            let weightToGrams = Weight.value

            let deserializeOption f = function
                | Some v -> v |> f <!> Some
                | _ -> Ok None

        let email: Email -> Result<Email, EmailError> = Email.value >> Email.create

        let credentials: Username * Shared.Password -> Result<Credentials, CredentialsError> = function
            | Username "", Password "" -> Error EmptyCredentials
            | Username "", _ -> Error EmptyUsername
            | _, Password "" -> Error EmptyPassword
            | username, password -> Ok { Username = username; Password = password |> Password.ofUserInput }

        let private ownership: Dto.Common.OwnershipStatus -> OwnershipStatus = function
            | Dto.Common.OwnershipStatus.Own -> Own
            | Dto.Common.OwnershipStatus.Wish -> Wish
            | Dto.Common.OwnershipStatus.Maybe -> Maybe
            | Dto.Common.OwnershipStatus.Idea -> Idea
            | Dto.Common.OwnershipStatus.ToBuy -> ToBuy
            | Dto.Common.OwnershipStatus.ToSell -> ToSell
            | Dto.Common.OwnershipStatus.Ordered -> Ordered

        let private size: Dto.Common.Weight option * Dto.Common.Dimensions option -> Result<Size, _> = fun (w, d) -> result {
            return {
                Weight = w |> Option.map (weightToGrams >> Weight.ofGrams)
                Dimensions = None   // todo - parse dimensions
            }
        }

        let private color: Dto.Common.Color -> Result<Color, _> = fun (Dto.Common.Color color) -> result {
            return Color color    // todo - parse
        }

        let private tag: Dto.Common.Tag -> Result<Tag, _> = fun { Slug = Dto.Common.Slug slug; Name = Dto.Common.TagName tag } -> result {
            let! parsed =
                Tag.parse tag
                |> Result.ofOption "Is not valid tag."

            if (parsed.Slug |> Slug.value) <> slug then
                return! Error <| sprintf "Tag has a different slug as it should have. Actual %A, expected %A." (parsed.Slug |> Slug.value) slug

            return parsed
        }

        let private link: Dto.Common.Link -> Result<Link, _> = fun (Dto.Common.Link link) ->
            link
            |> Link.parse <@> LinkError.format

        let private ean: Dto.Common.Ean -> Result<Ean, _> = fun (Dto.Common.Ean ean) -> result {
            return Ean ean    // todo - parse
        }

        let private price: Dto.Common.Price -> Result<Price, _> =
            fun { Amount = amount; Currency = currency } -> result {
                // currency |> Currency.parse // todo

                return {
                    Amount = amount
                    Currency = Currency.Czk
                }
            }

        let private productInfo: Dto.Common.ProductInfo -> Result<ProductInfo, _> =
            fun product -> result {
                let id =
                    product.Id
                    |> Id.tryParse
                    |> Option.defaultValue (Id.create())

                let name = product.Name
                let! manufacturer =
                    product.Manufacturer
                    |> Manufacturer.parse
                    |> Result.ofOption "Wrong manufacturer" // todo error

                let! price = product.Price |> price
                let! ean = product.Ean |> deserializeOption ean
                let! links =
                    product.Links
                    |> List.map link
                    |> Result.sequence

                return {
                    Id = id
                    Name = name
                    Manufacturer = manufacturer
                    Price = price
                    Ean = ean
                    Links = links
                }
            }

        let private gallery: Dto.Common.Gallery -> Result<Gallery, _> =
            fun gallery -> result {
                let! images =
                    gallery.Images
                    |> List.map link
                    |> Result.sequence

                return {
                    Images = images
                }
            }

        let private commonInfo: Dto.Common.CommonInfo -> Result<CommonInfo, _> = fun info -> result {
            let name = info.Name    // todo - validate, trim, ...
            let note = info.Note    // todo - validate, trim, ...
            let color = None // info.Color |> Option.map color
            let! tags =
                info.Tags
                |> List.map tag
                |> Result.sequence
            let! links =
                info.Links
                |> List.map link
                |> Result.sequence
            let price = None // info.Price |> Option.map price
            let size = None // info.Size |> Option.map size
            let ownershipStatus = info.OwnershipStatus |> ownership
            let! product = info.Product |> deserializeOption productInfo
            let gallery = None // info.Gallery |> Option.map gallery

            return {
                Name = name
                Note = note
                Color = color
                Tags = tags
                Links = links
                Price = price
                Size = size
                OwnershipStatus = ownershipStatus
                Product = product
                Gallery = gallery
            }
        }

        let private toolInfo: Dto.Items.ToolInfo -> Result<ToolInfo, _> = fun info -> result {
            let! common = info.Common |> commonInfo

            return {
                Common = common
            }
        }

        let private tool: Dto.Items.Tool -> Result<Tool, _> = function
            | Dto.Items.MultiTool multiTool -> multiTool |> toolInfo <!> MultiTool
            | Dto.Items.Knife knife -> knife |> toolInfo <!> Knife
            | Dto.Items.Gun gun -> gun |> toolInfo <!> Gun
            | Dto.Items.OtherTool other -> other |> toolInfo <!> OtherTool

        let private consumableInfo: Dto.Items.ConsumableInfo -> Result<ConsumableInfo, _> = fun info -> result {
            let! common = info.Common |> commonInfo

            return {
                Common = common
            }
        }

        let private consumable: Dto.Items.Consumable -> Result<Consumable, _> = function
            | Dto.Items.Food food -> food |> consumableInfo <!> Food
            | Dto.Items.OtherConsumable other -> other |> consumableInfo <!> OtherConsumable

        let rec item: Dto.Items.Item -> Result<Item, _> = function  // todo - error
            | Dto.Items.Item.Tool t -> t |> tool <!> Item.Tool
            | Dto.Items.Item.Container c -> c |> container <!> Item.Container
            | Dto.Items.Item.Consumable c -> c |> consumable <!> Item.Consumable

        and itemEntity: Dto.Items.ItemEntity -> Result<ItemEntity, _> =
            fun { Id = (Dto.Common.Id id); Item = i } -> result {
                let! id =
                    id
                    |> Id.tryParse
                    |> Result.ofOption "... invalid id ..."

                let! item = i |> item

                return {
                    Id = id
                    Item = item
                }
            }

        and private itemInContainer: Dto.Items.ItemInContainer -> Result<ItemInContainer, _> =
            fun { Item = ie; Quantity = q } -> result {
                let! item = ie |> itemEntity

                return {
                    Item = item
                    Quantity = q
                }
            }

        and private containerInfo: Dto.Items.ContainerInfo -> Result<ContainerInfo, _> =
            fun info -> result {
                let! common = info.Common |> commonInfo
                let! items =
                    info.Items
                    |> List.map itemInContainer
                    |> Result.sequence

                let! totalSize = (Some info.TotalWeight, None) |> size

                return {
                    Common = common

                    Items = items
                    TotalSize = totalSize
                }
            }

        and private container: Dto.Items.Container -> Result<Container, _> = function
            | Dto.Items.BagPack bagPack -> bagPack |> containerInfo <!> BagPack
            | Dto.Items.Organizer organizer -> organizer |> containerInfo <!> Organizer
            | Dto.Items.Pocket pocket -> pocket |> containerInfo <!> Pocket
            | Dto.Items.Panel panel -> panel |> containerInfo <!> Panel
            | Dto.Items.OtherContainer other -> other |> containerInfo <!> OtherContainer
