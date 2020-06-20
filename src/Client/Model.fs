module Model

open Elmish
open Elmish.UrlParser
open Elmish.Navigation

open Thoth.Json

open PageLoginModule
open PageEdcModule
open PageItemsModule
open PageAddItemModule

open Shared
open ProfilerModel
open Shared.Dto.Login
open Shared.Dto.Common
open Shared.Dto.Items
open Shared.Dto.Edc

type Page =
    // Public
    | Login
    | AnonymousEdcSets of Id option

    // Secured
    | MyEdcSets of Id option
    | Items of Id option
    | AddItem

[<RequireQualifiedAccess>]
module Page =
    [<RequireQualifiedAccess>]
    type private Pages =
        | Login
        | AnonymousEdcSets
        | MyEdcSets
        | Items
        | AddItem

    [<RequireQualifiedAccess>]
    module private Pages =
        let ofPage = function
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
        | Login
        | AnonymousEdcSets None
        | MyEdcSets None
        | Items None
        | AddItem -> pagesDefinitions |> PagesDefinitions.pages

    let toPath page =
        let p, detail = Pages.ofPage page

        match detail, pagesMap.[p] with
        | Some id, { DetailPath = Some detail } -> detail (id |> Id.value)
        | _, definition -> definition.Path
        |> sprintf "#/%s"

    let parse: Parser<Page -> Page, Page> =
        oneOf (pagesDefinitions |> List.collect (snd >> PageDefinition.parsers))

/// The model holds data that you want to keep track of while the application is running
type Model = {
    Success: SuccessMessage list
    Errors: ErrorMessage list

    CurrentPage: Page
    CurrentUser: User option

    Profiler: ProfilerModel

    PageLogin: PageLoginModel
    PageAnonymousEdcModel: PageEdcModel
    PageMyEdcModel: PageEdcModel
    PageItemsModel: PageItemsModel
    PageAddItemModel: PageAddItemModel
}

[<RequireQualifiedAccess>]
module Model =
    let urlUpdate (page: Page option) model =
        match page with
        | Some page -> { model with CurrentPage = page }, Cmd.none
        | _ -> model, Navigation.modifyUrl (Page.toPath model.CurrentPage)

//
// Messages / Actions
//

type PageAction =
    | GoToLogin
    | Logout
    | GoToAnonymousEdcSets
    | GoToMyEdcSets
    | GoToItems
    | GoToAddItem

/// The Action type defines what events/actions can occur while the application is running
/// the state of the application changes *only* in reaction to these events
type Action =
    | ShowSuccess of SuccessMessage
    | HideSuccess
    | ShowError of ErrorMessage
    | HideErrors

    | LoggedIn of User
    | LoggedOut
    | LoggedOutWithError of ErrorMessage

    | ProfilerAction of ProfilerAction

    | PageAction of PageAction
    | PageLoginAction of PageLoginAction
    | PageAnonymousEdcAction of PageEdcAction
    | PageMyEdcAction of PageEdcAction
    | PageItemsAction of PageItemsAction
    | PageAddItemAction of PageAddItemAction

type Dispatch = Action -> unit

let initialModel = {
    Success = []
    Errors = []

    CurrentPage = Login
    CurrentUser = None

    Profiler = ProfilerModel.empty

    PageLogin = PageLoginModel.empty
    PageAnonymousEdcModel = PageEdcModel.empty
    PageMyEdcModel = PageEdcModel.empty
    PageItemsModel = PageItemsModel.empty
    PageAddItemModel = PageAddItemModel.empty
}

let pageInitAction = function
    | Login -> Cmd.ofMsg (PageLoginAction.InitPage |> PageLoginAction)
    | AnonymousEdcSets set -> Cmd.ofMsg (PageEdcAction.InitPage set |> PageMyEdcAction)
    | MyEdcSets set -> Cmd.ofMsg (PageEdcAction.InitPage set |> PageMyEdcAction)
    | Items item -> Cmd.ofMsg (PageItemsAction.InitPage item |> PageItemsAction)
    | AddItem -> Cmd.ofMsg (PageAddItemAction.InitPage |> PageAddItemAction)

//
// Interval tasks
// todo<later> - try to replace them by sockets
//

let mutable refreshingProfilerStarted = false
let refreshProfiler dispatch =
    if not refreshingProfilerStarted then
        async {
            printfn "[Profiler] Refresh every 60s ..."

            while true do
                let! action =
                    LocalStorage.Key.ProfilerToken
                    |> LocalStorage.load (Decode.string)
                    |> Result.toOption
                    |> Option.map Profiler.Token
                    |> Api.loadProfiler (ProfilerAction.ShowProfiler >> ProfilerAction)
                dispatch action

                do! Async.Sleep (60 * 1000)
        }
        |> Async.StartImmediate
    refreshingProfilerStarted <- true

/// defines the initial state and initial command (= side-effect) of the application
let init page : Model * Cmd<Action> =
    let user = User.load()

    let model, cmd =
        match user with
        | Some loggedInUser ->
            let page = page |> Option.defaultValue (AnonymousEdcSets None)
            { initialModel with CurrentUser = Some loggedInUser; CurrentPage = page } |> Model.urlUpdate None
        | _ ->
            { initialModel with CurrentPage = Login } |> Model.urlUpdate None

    model, Cmd.batch [
        yield cmd

        if model.CurrentUser.IsSome then
            yield! Page.all model.CurrentPage |> List.map pageInitAction

        yield
            LocalStorage.Key.ProfilerToken
            |> LocalStorage.load (Decode.string)
            |> Result.toOption
            |> Option.map Profiler.Token
            |> Api.loadProfiler (ProfilerAction.ShowProfiler >> ProfilerAction)
            |> Cmd.OfAsyncImmediate.result
    ]

let private modifyUrl page =
    page
    |> Page.toPath
    |> Navigation.modifyUrl

let (|IsLoggedIn|_|) = function
    | { CurrentUser = Some _ } -> Some IsLoggedIn
    | _ -> None

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (action : Action) (model : Model) : Model * Cmd<Action> =
    match action with
    | PageAction pageAction ->
        let goTo page =
            { model with CurrentPage = page }, Cmd.batch [
                Navigation.newUrl (Page.toPath page)
                page |> pageInitAction
            ]

        match pageAction, model with
        | Logout, IsLoggedIn ->
            model, Cmd.OfFunc.either User.delete () (fun _ -> LoggedOut) (ErrorMessage.fromExn >> ShowError)

        | GoToAnonymousEdcSets, _ -> AnonymousEdcSets model.PageAnonymousEdcModel.SelectedSet |> goTo
        | GoToMyEdcSets, IsLoggedIn -> MyEdcSets model.PageMyEdcModel.SelectedSet |> goTo
        | GoToItems, IsLoggedIn -> Items model.PageItemsModel.ItemDetail |> goTo
        | GoToAddItem, IsLoggedIn -> AddItem |> goTo

        | GoToLogin, IsLoggedIn
        | _, { CurrentUser = None } -> goTo Login

        | _ -> model, Cmd.none

    // Global Messages
    | ShowSuccess message ->
        { model with Success = message :: model.Success },
        Cmd.batch [
            Cmd.OfAsyncImmediate.result (async {
                do! Async.Sleep (10 * 1000)
                // todo - debounce hiding (keep each success with own timer)
                return HideSuccess
            })
        ]
    | HideSuccess -> { model with Success = [] }, Cmd.none

    | ShowError error ->
        { model with Errors = error :: model.Errors |> List.distinct },
        Cmd.OfAsyncImmediate.result (async {
            do! Async.Sleep (10 * 1000)
            // todo - debounce hiding errors (keep each error with own timer)
            return HideErrors
        })
    | HideErrors -> { model with Errors = [] }, Cmd.none

    // Login
    | LoggedIn user ->
        { model with CurrentUser = Some user }, Cmd.batch [
            yield! Page.all (MyEdcSets None) |> List.map pageInitAction

            yield GoToMyEdcSets |> PageAction |> Cmd.ofMsg
            yield SuccessMessage "You are successfully logged in." |> ShowSuccess |> Cmd.ofMsg
        ]

    | LoggedOut ->
        initialModel, Cmd.batch [
            Cmd.ofMsg (ShowSuccess (SuccessMessage "You have been successfuly logged out."))
            Cmd.ofMsg (PageAction GoToLogin)
        ]
    | LoggedOutWithError error ->
        initialModel, [
            ShowError error
            ShowError (ErrorMessage "You have been logged out.")
            PageAction GoToLogin
        ]
        |> List.map Cmd.ofMsg
        |> Cmd.batch

    // Profiler
    | ProfilerAction action ->
        let profiler, action = action |> ProfilerModel.update model.Profiler
        { model with Profiler = profiler }, action

    //
    // Pages
    //
    | PageLoginAction pageAction ->
        let loginAction =
            match pageAction with
            | PageLoginAction.LoginSuccess newUser ->
                Cmd.OfFunc.either User.save newUser (fun _ -> LoggedIn newUser) (ErrorMessage.fromExn >> ShowError)
                |> Some
            | _ -> None

        let pageModel, action =
            pageAction
            |> PageLoginModel.update
                PageLoginAction
                ShowSuccess
                ShowError
                model.PageLogin

        { model with PageLogin = pageModel }, [
            loginAction
            Some action
        ]
        |> List.choose id
        |> Cmd.batch

    | PageAnonymousEdcAction pageAction ->
        let updatedPage =
            match pageAction with
            | PageEdcAction.SelectSet (Some set) -> Some (AnonymousEdcSets (Some set))
            | _ -> None

        let pageModel, action =
            pageAction
            |> PageEdcModel.update
                PageMyEdcAction
                ShowSuccess
                ShowError
                LoggedOutWithError
                model.PageAnonymousEdcModel

        { model with PageAnonymousEdcModel = pageModel }, [
            Some action
            updatedPage |> Option.map modifyUrl
        ]
        |> List.choose id
        |> Cmd.batch

    | PageMyEdcAction pageAction ->
        let updatedPage =
            match pageAction with
            | PageEdcAction.SelectSet (Some set) -> Some (MyEdcSets (Some set))
            | _ -> None

        let pageModel, action =
            pageAction
            |> PageEdcModel.update
                PageMyEdcAction
                ShowSuccess
                ShowError
                LoggedOutWithError
                model.PageMyEdcModel

        { model with PageMyEdcModel = pageModel }, [
            Some action
            updatedPage |> Option.map modifyUrl
        ]
        |> List.choose id
        |> Cmd.batch

    | PageItemsAction pageAction ->
        let updatedPage =
            match pageAction with
            | PageItemsAction.ShowDetail id -> Some (Items (Some id))
            | PageItemsAction.HideDetail -> Some (Items None)
            | _ -> None

        let pageModel, action =
            pageAction
            |> PageItemsModel.update
                PageItemsAction
                ShowSuccess
                ShowError
                LoggedOutWithError
                model.PageItemsModel

        { model with PageItemsModel = pageModel }, [
            Some action
            updatedPage |> Option.map modifyUrl
        ]
        |> List.choose id
        |> Cmd.batch

    | PageAddItemAction pageAction ->
        let redirect =
            match pageAction with
            | PageAddItemAction.ItemSaved _item -> Some (PageAction GoToItems)
            | _ -> None

        let pageModel, action =
            pageAction
            |> PageAddItemModel.update
                PageAddItemAction
                ShowSuccess
                ShowError
                LoggedOutWithError
                model.PageAddItemModel

        { model with PageAddItemModel = pageModel }, [
            Some action
            redirect |> Option.map Cmd.ofMsg
        ]
        |> List.choose id
        |> Cmd.batch
