[<RequireQualifiedAccess>]
module PageEdcSets

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open PageEdcModule
open Shared.Dto.Edc

let page (model: PageEdcModel) (dispatch: DispatchPageEdcAction) =
    div [] [
        str "Todo ... "
        pre [] [ str (sprintf "%A" model) ]
    ]
