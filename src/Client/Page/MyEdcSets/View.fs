[<RequireQualifiedAccess>]
module PageMyEdcSets

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open PageMyEdcModule
open Shared.Dto.Edc

let page (model: PageMyEdcModel) (dispatch: DispatchPageMyEdcAction) =
    div [] [
        str "Todo ... "
        pre [] [ str (sprintf "%A" model) ]
    ]
