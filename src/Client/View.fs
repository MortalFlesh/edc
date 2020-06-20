module Client

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared
open Model

let private globalMessages model =
    fragment [] [
        if model.Success |> List.isEmpty |> not then
            Columns.columns [] [
                model.Success
                |> List.map (fun (SuccessMessage message) ->
                    Notification.notification [ Notification.Color IsSuccess ] [ str message ]
                )
                |> Column.column [ Column.Width (Screen.All, Column.Is12) ]
            ]

        if model.Errors |> List.isEmpty |> not then
            Columns.columns [] [
                model.Errors
                |> List.map (fun (ErrorMessage error) ->
                    Notification.notification [ Notification.Color IsDanger ] [ str error ]
                )
                |> Column.column [ Column.Width (Screen.All, Column.Is12) ]
            ]
    ]

let view (model: Model) (dispatch: Dispatch) =
    let routing = Routing.routing (PageAction >> dispatch)

    div [] [
        Navbar.navbarView routing (fun _ -> dispatch OpenBurgerMenu) (fun _ -> dispatch CloseBurgerMenu) model.CurrentPage model.CurrentUser model.BurgerMenu

        Container.container [] [
            globalMessages model

            match model.CurrentPage with
            | Login -> PageLogin.page model.PageLogin (PageLoginAction >> dispatch)
            | AnonymousEdcSets _ -> PageEdcSets.page model.PageAnonymousEdcModel (PageAnonymousEdcAction >> dispatch)
            | MyEdcSets _ -> PageEdcSets.page model.PageMyEdcModel (PageMyEdcAction >> dispatch)
            | Items _ -> PageItems.page routing model.PageItemsModel (PageItemsAction >> dispatch)
            | AddItem _ -> PageAddItem.page model.PageAddItemModel (PageAddItemAction >> dispatch)
        ]

        Profiler.profiler (fun () -> refreshProfiler dispatch) model.Profiler (ProfilerAction >> dispatch)
    ]

open Elmish.UrlParser
open Elmish.HMR
#if DEBUG
open Elmish.Debug
#endif

Program.mkProgram init update view
|> Program.toNavigable (parseHash Page.parse) Model.urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
