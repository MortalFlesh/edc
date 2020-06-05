module Component.Common

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json
open Fulma.Extensions.Wikiki

open Shared
open Shared.Dto.Common

[<RequireQualifiedAccess>]
module Link =
    let link (Link link) =
        a [ Href link; Target "_blank" ] [ str link ]

[<RequireQualifiedAccess>]
module Tag =
    let link (Tag tag) =
        a [ (* Todo - goToTagDetail tag *) ] [ str tag ]

