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
open Shared.Dto.Edc

let page (model: PageItemsModel) (dispatch: DispatchPageItemsAction) =
    div [] [
        str "Todo ... "
        pre [] [ str (sprintf "%A" model) ]
    ]
