module Model

open Elmish
open Elmish.UrlParser
open Elmish.Navigation

open Thoth.Json

open PageLoginModule
open PageEdcModule
open PageItemsModule

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

[<RequireQualifiedAccess>]
module Page =
    let all = function
        | MyEdcSets (Some set) ->
            [
                MyEdcSets (Some set)
            ]
        | _ ->
            [
                MyEdcSets None
            ]

    let toPath =
        function
        | Login -> "login"

        | AnonymousEdcSets (Some set) -> sprintf "sets/%s" (set|> Id.value)
        | AnonymousEdcSets None -> "sets"

        | MyEdcSets (Some set) -> sprintf "my-sets/%s" (set|> Id.value)
        | MyEdcSets None -> "my-sets"

        | Items (Some item) -> sprintf "items/%s" (item|> Id.value)
        | Items None -> "items"
        >> (+) "#/"

    let parse: Parser<Page -> Page, Page> =
        oneOf [
            map Login (s "login")

            map (Id.parse >> AnonymousEdcSets) (s "sets" </> str)
            map (AnonymousEdcSets None) (s "sets")

            map (Id.parse >> MyEdcSets) (s "my-sets" </> str)
            map (MyEdcSets None) (s "my-sets")

            map (Id.parse >> Items) (s "items" </> str)
            map (Items None) (s "my-sets")
        ]

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
}

let pageInitAction = function
    | Login -> Cmd.ofMsg (PageLoginAction.InitPage |> PageLoginAction)
    | AnonymousEdcSets set -> Cmd.ofMsg (PageEdcAction.InitPage set |> PageMyEdcAction)
    | MyEdcSets set -> Cmd.ofMsg (PageEdcAction.InitPage set |> PageMyEdcAction)
    | Items item -> Cmd.ofMsg (PageItemsAction.InitPage item |> PageItemsAction)

//
// Interval tasks
// todo<later> - try to replace them by sockets
//

let mutable refreshingProfilerStarted = false
let refreshProfiler dispatch =
    if not refreshingProfilerStarted then
        async {
            printfn "[Profiler] Refresh every 15s ..."

            while true do
                let! action =
                    LocalStorage.Key.ProfilerToken
                    |> LocalStorage.load (Decode.string)
                    |> Result.toOption
                    |> Option.map Profiler.Token
                    |> Api.loadProfiler (ProfilerAction.ShowProfiler >> ProfilerAction)
                dispatch action

                do! Async.Sleep (15 * 1000)
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

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (action : Action) (model : Model) : Model * Cmd<Action> =
    match action with
    | PageAction pageAction ->
        match pageAction, model with
        | GoToLogin, { CurrentUser = None } ->
            { model with CurrentPage = Page.Login }, Navigation.newUrl (Page.toPath Page.Login)

        | Logout, { CurrentUser = Some _ } ->
            model, Cmd.OfFunc.either User.delete () (fun _ -> LoggedOut) (ErrorMessage.fromExn >> ShowError)

        | GoToAnonymousEdcSets, _ ->
            let page = AnonymousEdcSets model.PageAnonymousEdcModel.SelectedSet
            { model with CurrentPage = page }, Navigation.newUrl (Page.toPath page)

        | GoToMyEdcSets, { CurrentUser = Some _ } ->
            let page = MyEdcSets model.PageMyEdcModel.SelectedSet
            { model with CurrentPage = page }, Navigation.newUrl (Page.toPath page)

        | GoToItems, { CurrentUser = Some _ } ->
            let page = Items model.PageItemsModel.ItemDetail
            { model with CurrentPage = page }, Navigation.newUrl (Page.toPath page)

        | _, { CurrentUser = None } -> model, Cmd.ofMsg (PageAction GoToLogin)

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
        let navigateAction =
            match pageAction with
            | PageEdcAction.SelectSet (Some set) ->
                AnonymousEdcSets (Some set)
                |> Page.toPath
                |> Navigation.modifyUrl
                |> Some
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
            navigateAction
        ]
        |> List.choose id
        |> Cmd.batch

    | PageMyEdcAction pageAction ->
        let navigateAction =
            match pageAction with
            | PageEdcAction.SelectSet (Some set) ->
                MyEdcSets (Some set)
                |> Page.toPath
                |> Navigation.modifyUrl
                |> Some
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
            navigateAction
        ]
        |> List.choose id
        |> Cmd.batch

    | PageItemsAction pageAction ->
        let navigateAction =
            match pageAction with
            | PageItemsAction.ShowDetail id ->
                Items (Some id)
                |> Page.toPath
                |> Navigation.modifyUrl
                |> Some
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
            navigateAction
        ]
        |> List.choose id
        |> Cmd.batch
