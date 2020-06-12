module Component.FlatItems

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json
open Fulma.Extensions.Wikiki

open Component.Common

open Shared
open Shared.FlatItem

[<RequireQualifiedAccess>]
module FlatItem =
    let types fItem =
        match fItem.SubType with
        | Some subType -> sprintf "%s (%s)" fItem.Type subType
        | _ -> fItem.Type

    let body fItem =
        div [] [    // todo colorize?
            ul [] [
                li [] [ str (fItem |> types) ]

                match fItem.Common.Note with
                | Some note -> li [] [ str note ]
                | _ -> ()

                (*
                    Tags: Tag list
                    Links: Link list
                    Price: Price option
                    Size: Size option
                    OwnershipStatus: OwnershipStatus
                    Product: ProductInfo option
                    Gallery: Gallery option *)

                yield! fItem.Common.Tags |> List.map (fun t -> li [] [ t |> Tag.link ])
                yield! fItem.Common.Links |> List.map (fun l -> li [] [ l |> Link.link ])
            ]

            match fItem.Common.Size with
            // todo - move to Size component
            | Some size ->
                fieldset [] [
                    legend [] [
                        str "Size"
                    ]

                    match size.Weight with
                    | Some weight -> div [] [ weight |> sprintf "Weight: %Ag" |> str ]
                    | _ -> ()

                    match size.Dimensions with
                    | Some { Height = height; Width = width; Length = length } -> div [] [ sprintf "Dimensions (H*W*L): %Amm * %Amm * %Amm" height width length |> str ]
                    | _ -> ()
                ]
            | _ -> ()
        ]

    let preview hide isActive (fItem: FlatItemData<_>) =
        let hide = fun _ -> hide ()

        Quickview.quickview [ Quickview.IsActive isActive ] [
            Quickview.header [] [
                Quickview.title [] [ str fItem.Common.Name ]
                Delete.delete [ Delete.OnClick hide ] []
            ]

            Quickview.body [] [
                fItem |> body
            ]

            Quickview.footer [] [
                Button.button [ (* Button.OnClick (goToItemDetail id) *) ] [ str "Detail" ] // todo - implement item detail
                Button.button [ Button.OnClick hide ] [ str "Hide preview" ]
            ]
        ]

[<RequireQualifiedAccess>]
module FlatItems =
    let table showPreview hidePreview previewedId (items: FlatItemEntity<_> list) =
        fragment [] [
            yield!
                items
                |> List.map (fun { Id = id; Item = item } ->
                    item |> FlatItem.preview hidePreview (previewedId = Some id)
                )

            items
            |> Component.table [ "Name"; "Item Type (Subtype)" ] (fun { Id = id; Item = fItem } ->
                [
                    a [
                        OnClick (fun _ ->
                            if previewedId = Some id
                                then hidePreview ()
                                else showPreview id)
                    ] [ str fItem.Common.Name ]

                    fItem |> FlatItem.types |> str
                ]
            )
        ]
