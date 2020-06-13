[<RequireQualifiedAccess>]
module Tag

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

let deleteable onDelete color tagName =
    Tag.tag
        [ Tag.Color color ]
        [
            str tagName
            Delete.delete [
                Delete.Size IsSmall
                Delete.OnClick (fun _ -> onDelete tagName)
            ] []
        ]

let link { Slug = Slug slug; Name = TagName tag } =
    Tag.tag
        [ Tag.Color IsPrimary ]
        [
            a [ (* Todo - goToTagDetail tag.Slug *) ] [ str tag ]
        ]

let list tags =
    tags
    |> List.map link
    |> div []
