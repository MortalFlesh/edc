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

let private isSelected = function
    | AnonymousEdcSets _, AnonymousEdcSets _ -> true
    | MyEdcSets _, MyEdcSets _ -> true
    | Items _, Items _ -> true
    | _ -> false

let private navBrand routing { CurrentPage = page; CurrentUser = user } =
    Navbar.navbar [ Navbar.Color IsPrimary ] [
        Container.container [] [
            Navbar.Brand.div [] [
                Navbar.Item.a [ Navbar.Item.CustomClass "brand-text" ] [ str "EDC" ]
            ]

            Navbar.menu [] [
                Navbar.Start.div [] [
                    Navbar.Item.a [
                        Navbar.Item.IsActive (isSelected (page, MyEdcSets None))
                        Navbar.Item.Props [ OnClick (fun _ -> routing.GoToMyEdcSets()) ]
                    ] [ str "My Sets" ]

                    Navbar.Item.a [
                        Navbar.Item.IsActive (isSelected (page, AnonymousEdcSets None))
                        Navbar.Item.Props [ OnClick (fun _ -> routing.GoToAnonymousEdcSets()) ]
                    ] [ str "Sets" ]

                    Navbar.Item.a [
                        Navbar.Item.IsActive (isSelected (page, Items None))
                        Navbar.Item.Props [ OnClick (fun _ -> routing.GoToItems()) ]
                    ] [ str "Items" ]

                    Navbar.Item.a [
                        (* todo *)
                    ] [ str "Containers" ]

                    Navbar.Item.a [
                        (* todo *)
                    ] [ str "Tags" ]

                    Navbar.Item.a [
                        (* todo *)
                    ] [ str "Stats" ]

                    Navbar.Item.a [
                        (* todo *)
                    ] [ str "Wishlist" ]
                ]
            ]

            Navbar.End.div [] [
                match user with
                | Some { Username = Username username } ->
                    Navbar.Item.a [
                        Navbar.Item.IsActive true
                    ] [
                        Component.Icon.medium Fa.Solid.UserShield
                        str username
                    ]

                    Navbar.Item.a [
                        Navbar.Item.Props [ OnClick (fun _ -> routing.Logout()) ]
                    ] [
                        str "Log out"
                        Component.Icon.medium Fa.Solid.SignOutAlt
                    ]
                | _ ->
                    Navbar.Item.a [
                        Navbar.Item.IsActive true
                        Navbar.Item.Props [ OnClick (fun _ -> routing.GoToLogin()) ]
                    ] [ em [] [ str "Anonymous" ] ]
            ]
        ]
    ]

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

    Columns.columns [] [
        Column.column [ Column.Width (Screen.All, Column.Is12) ] [
            navBrand routing model

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
