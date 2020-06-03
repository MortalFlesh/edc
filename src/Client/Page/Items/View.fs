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

open Component.Items

let page (model: PageItemsModel) (dispatch: DispatchPageItemsAction) =
    div [] [
        Component.subTitle "Items"

        model.Items
        |> List.map ItemEntity.item
        |> Items.commonTable

        hr []
        pre [] [ str (sprintf "%A" model) ]
    ]
