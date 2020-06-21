[<AutoOpen>]
module Routing

open Model

let routing dispatch = {
    GoToLogin = fun () -> dispatch GoToLogin
    Logout = fun () -> dispatch Logout
    GoToAnonymousEdcSets = fun () -> dispatch GoToAnonymousEdcSets
    GoToMyEdcSets = fun () -> dispatch GoToMyEdcSets
    GoToItems = fun () -> dispatch GoToItems
    GoToAddItem = fun () -> dispatch GoToAddItem
}
