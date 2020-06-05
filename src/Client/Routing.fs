[<AutoOpen>]
module Routing

type Routing = {
    GoToLogin: unit -> unit
    Logout: unit -> unit
    GoToAnonymousEdcSets: unit -> unit
    GoToMyEdcSets: unit -> unit
    GoToItems: unit -> unit
    GoToAddItem: unit -> unit
}

open Model

let routing dispatch = {
    GoToLogin = fun () -> dispatch GoToLogin
    Logout = fun () -> dispatch Logout
    GoToAnonymousEdcSets = fun () -> dispatch GoToAnonymousEdcSets
    GoToMyEdcSets = fun () -> dispatch GoToMyEdcSets
    GoToItems = fun () -> dispatch GoToItems
    GoToAddItem = fun () -> dispatch GoToAddItem
}
