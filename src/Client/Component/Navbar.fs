[<RequireQualifiedAccess>]
module Navbar

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared
open Shared.Dto.Login

// see https://github.com/MangelMaxime/fulma-demo/blob/master/src/App.fs

let private isSelected = function
    | AnonymousEdcSets _, AnonymousEdcSets _ -> true
    | MyEdcSets _, MyEdcSets _ -> true
    | Items _, Items _ -> true
    | _ -> false

let private avatar (* todo - userAvatar *) =
    Component.Icon.medium Fa.Solid.UserNinja

let private profileItemEnd page routing = function
    | Some { Username = Username username } ->
        fragment [] [
            Navbar.Item.a [
                // todo - show user notifications
                Navbar.Item.CustomClass "is-hidden-touch"
            ] [ Component.Icon.medium Fa.Solid.Bell ]

            Navbar.Item.div [ Navbar.Item.HasDropdown; Navbar.Item.IsHoverable ] [
                Navbar.Item.div [ Navbar.Item.IsActive true ] [
                    avatar
                    str username
                ]

                Navbar.Dropdown.div [ Navbar.Dropdown.CustomClass "navbar-dropdown--right" ] [
                    Navbar.Item.a [
                        Navbar.Item.Props [ OnClick (ignore) (* todo *) ]
                    ] [ str "Profile" ]

                    Navbar.Item.a [
                        Navbar.Item.IsActive (isSelected (page, MyEdcSets None))
                        Navbar.Item.Props [ OnClick (fun _ -> routing.GoToMyEdcSets()) ]
                    ] [ str "My Sets" ]

                    Navbar.Item.a [
                        Navbar.Item.Props [ OnClick (ignore) (* todo *) ]
                    ] [ str "My items" ]

                    Navbar.divider [] []

                    Navbar.Item.a [
                        Navbar.Item.Props [ OnClick (fun _ -> routing.Logout()) ]
                    ] [
                        str "Log out"
                        Component.Icon.medium Fa.Solid.SignOutAlt
                    ]
                ]
            ]
        ]
    | _ ->
        fragment [] [
            Navbar.Item.a [
                Navbar.Item.IsActive true
                Navbar.Item.Props [ OnClick (fun _ -> routing.GoToJoin()) ]
            ] [ str "Join" ]

            Navbar.Item.a [
                Navbar.Item.Props [ OnClick (fun _ -> routing.GoToLogin()) ]
            ] [
                em [] [ str "Log in" ]
                Component.Icon.medium Fa.Solid.User
            ]
        ]

let private profileItemBrand routing =
    let hiddenOnDesktop = Navbar.Item.CustomClass "is-hidden-desktop"

    function
    | Some _ ->
        fragment [] [
            Navbar.Item.a [
                hiddenOnDesktop
                // todo - show user notifications
            ] [ Component.Icon.medium Fa.Solid.Bell ]

            Navbar.Item.a [
                hiddenOnDesktop
                // todo - go to profile
            ] [ avatar ]
        ]
    | _ ->
        fragment [] [
            Navbar.Item.a [
                Navbar.Item.IsActive true
                Navbar.Item.Props [ OnClick (fun _ -> routing.GoToJoin()) ]
                hiddenOnDesktop
            ] [ str "Join" ]

            Navbar.Item.a [
                Navbar.Item.Props [ OnClick (fun _ -> routing.GoToLogin()) ]
                hiddenOnDesktop
            ] [
                em [] [ str "Log in" ]
                Component.Icon.medium Fa.Solid.User
            ]
        ]

let private navbarEnd routing page burgerMenu (user: User option) =
    Navbar.End.div [] [
        if burgerMenu = BurgerMenu.Closed then
            user |> profileItemEnd page routing
    ]

let private navbarStart routing page =
    Navbar.Start.div [] [
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

let private navbarBrand openBurgerMenu closeBurgerMenu routing burgerMenu (user: User option) =
    Navbar.Brand.div [] [
        Navbar.Item.a [ Navbar.Item.CustomClass "brand-text" ] [ str "EDC" ]

        Navbar.burger [
            Navbar.Burger.IsActive (burgerMenu = BurgerMenu.Opened)
            Navbar.Burger.Props [ Role "button"; AriaLabel "menu"; AriaExpanded false ]
            Navbar.Burger.OnClick (fun _ ->
                match burgerMenu with
                | BurgerMenu.Opened -> closeBurgerMenu()
                | BurgerMenu.Closed -> openBurgerMenu()
            )
        ] [
            span [ AriaHidden true ] []
            span [ AriaHidden true ] []
            span [ AriaHidden true ] []
        ]

        user |> profileItemBrand routing
    ]

let navbarView routing openBurgerMenu closeBurgerMenu currentPage user burgerMenu =
    Navbar.navbar [ Navbar.Color IsPrimary; Navbar.IsFixedTop ] [
        user |> navbarBrand openBurgerMenu closeBurgerMenu routing burgerMenu

        Navbar.menu [ Navbar.Menu.IsActive (burgerMenu = BurgerMenu.Opened) ] [
            currentPage |> navbarStart routing
            user |> navbarEnd routing currentPage burgerMenu
        ]
    ]
