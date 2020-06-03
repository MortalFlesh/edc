module Component.Items

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared
open Shared.Dto.Items

[<RequireQualifiedAccess>]
module Item =

    let view item =
        div [] []

    let viewEntity { Id = id; Item = item } =
        item |> view

    let common = function   // todo - create a type for this item format
        | Item.Tool tool ->
            "Tool",
            match tool with
            | MultiTool multiTool -> Some "MultiTool", multiTool.Common
            | Knife knife -> Some "Knife", knife.Common
            | Gun gun ->Some  "Gun", gun.Common
            | Tool.Other other -> None, other.Common

        | Item.Container container ->
            "Container",
            match container with
            | BagPack bagPack -> Some "BagPack", bagPack.Common
            | Organizer organizer -> Some "Organizer", organizer.Common
            | Pocket pocket -> Some "Pocket", pocket.Common
            | Panel panel -> Some "Panel", panel.Common
            | Other other -> None, other.Common

        | Item.Consumable consumable ->
            "Consumable",
            match consumable with
            | Food food -> Some "Food", food.Common
            | Consumable.Other other -> None, other.Common

    let entityRow { Id = id; Item = item } =
        let itemType, (subType, common) = item |> common

        tr [] [
            td [] [ str itemType ]
            td [] [ str (subType |> Option.defaultValue "") ]
            td [] [ str common.Name ]
        ]

[<RequireQualifiedAccess>]
module Items =
    let table headers row items =
        Table.table [
            Table.IsBordered
            Table.IsFullWidth
            Table.IsStriped
            Table.IsHoverable
        ] [
            thead [] [
                tr [] [
                    yield! headers |> List.map (fun header -> th [] [ str header ])
                ]
            ]

            tbody [] [
                yield! items |> List.map (Item.common >> row >> fun cols ->
                    tr [] [
                        yield! cols |> List.map (fun col -> td [] [ str col ])
                    ]
                )
            ]
        ]

    let commonTable =
        table [ "Name"; "Item Type (Subtype)" ] (fun (itemType, (subType, common)) ->
            [
                common.Name

                match subType with
                | Some subType -> sprintf "%s (%s)" itemType subType
                | _ -> itemType
            ]
        )
