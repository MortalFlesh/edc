[<RequireQualifiedAccess>]
module PageItems

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open PageItemsModule
open Shared.Dto.Items

open Component.FlatItems

let topMenu routing =
    Level.level [] [
        Level.left [] [
            Level.item [] [
                Component.subTitle "Items"
            ]
        ]

        Level.right [] [
            Level.item [] [
                Button.button [
                    Button.Color IsSuccess
                    Button.OnClick (fun _ -> routing.GoToAddItem())
                ] [
                    Component.Icon.medium Fa.Solid.PlusCircle
                    span [] [ str "Add" ]
                ]
            ]
        ]
    ]

let page routing (model: PageItemsModel) (dispatch: DispatchPageItemsAction) =
    div [] [
        topMenu routing

        model.Items
        |> List.map FlatItemEntity.ofItemEntity
        |> FlatItems.table
            (PageItemsAction.ShowDetail >> dispatch)
            (fun () -> PageItemsAction.HideDetail |> dispatch)
            model.ItemDetail

        hr []
        pre [] [ str (sprintf "%A" model) ]
    ]
