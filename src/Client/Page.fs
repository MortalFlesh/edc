[<AutoOpen>]
module PageModule

open Shared
open Shared.Dto.Common

type Page =
    // Public
    | Join
    | Login
    | AnonymousEdcSets of Id option

    // Secured
    | MyEdcSets of Id option
    | Items of Id option
    | AddItem

[<RequireQualifiedAccess>]
module Page =
    open Elmish.UrlParser

    [<RequireQualifiedAccess>]
    type private Pages =
        | Join
        | Login
        | AnonymousEdcSets
        | MyEdcSets
        | Items
        | AddItem

    [<RequireQualifiedAccess>]
    module private Pages =
        let ofPage = function
            | Join -> Pages.Join, None
            | Login -> Pages.Login, None
            | AnonymousEdcSets id -> Pages.AnonymousEdcSets, id
            | MyEdcSets id -> Pages.MyEdcSets, id
            | Items id -> Pages.Items, id
            | AddItem -> Pages.AddItem, None

    type private PageDefinition = {
        Path: string
        DetailPath: (string -> string) option
        Page: Page
        Parsers: Parser<Page -> Page, Page> list
    }

    [<RequireQualifiedAccess>]
    module private PageDefinition =
        let page { Page = page } = page
        let parsers { Parsers = parsers } = parsers

    [<RequireQualifiedAccess>]
    module private PagesDefinitions =
        let pages definitions =
            definitions
            |> List.map (snd >> PageDefinition.page)

    let private pagesDefinitions = [
        Pages.Join => {
            Path = "join"
            DetailPath = None
            Page = Join
            Parsers = [
                map Join (s "join")
            ]
        }
        Pages.Login => {
            Path = "login"
            DetailPath = None
            Page = Login
            Parsers = [
                map Login (s "login")
            ]
        }
        Pages.AnonymousEdcSets => {
            Path = "sets"
            DetailPath = Some (sprintf "sets/%s")
            Page = AnonymousEdcSets None
            Parsers = [
                map (Id.parse >> AnonymousEdcSets) (s "sets" </> str)
                map (AnonymousEdcSets None) (s "sets")
            ]
        }
        Pages.MyEdcSets => {
            Path = "my-sets"
            DetailPath = Some (sprintf "my-sets/%s")
            Page = MyEdcSets None
            Parsers = [
                map (Id.parse >> MyEdcSets) (s "my-sets" </> str)
                map (MyEdcSets None) (s "my-sets")
            ]
        }
        Pages.Items => {
            Path = "items"
            DetailPath = Some (sprintf "items/%s")
            Page = Items None
            Parsers = [
                map (Id.parse >> Items) (s "items" </> str)
                map (Items None) (s "items")
            ]
        }
        Pages.AddItem => {
            Path = "add-item"
            DetailPath = None
            Page = AddItem
            Parsers = [
                map AddItem (s "add-item")
            ]
        }
    ]

    let private pagesMap = Map.ofList pagesDefinitions

    let private withDetail detail page =
        pagesMap
        |> Map.add page { pagesMap.[page] with Page = detail }
        |> Map.toList
        |> List.map (snd >> PageDefinition.page)

    let all = function
        // Details
        | AnonymousEdcSets (Some _) as detail -> Pages.AnonymousEdcSets |> withDetail detail
        | MyEdcSets (Some _) as detail -> Pages.MyEdcSets |> withDetail detail
        | Items (Some _) as detail -> Pages.Items |> withDetail detail

        // List, Indexes, ...
        | Join
        | Login
        | AnonymousEdcSets None
        | MyEdcSets None
        | Items None
        | AddItem -> pagesDefinitions |> PagesDefinitions.pages

    //
    // Generic functions
    //

    let toPath page =
        let p, detail = Pages.ofPage page

        match detail, pagesMap.[p] with
        | Some id, { DetailPath = Some detail } -> detail (id |> Id.value)
        | _, definition -> definition.Path
        |> sprintf "#/%s"

    let parse: Parser<Page -> Page, Page> =
        oneOf (pagesDefinitions |> List.collect (snd >> PageDefinition.parsers))
