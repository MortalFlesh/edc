module MF.EDC.TagTest

open Expecto

type TagTestCase = {
    Description: string
    Original: string
    Expected: Tag option
}

let provideTags =
    [
        yield! [
            {
                Description = "Empty tag"
                Original = ""
                Expected = None
            }
            {
                Description = "With space"
                Original = "with space"
                Expected = None
            }
            {
                Description = "With leading & trailing space"
                Original = " word "
                Expected = Some { Slug = Slug "word"; Name = TagName "word" }
            }
            {
                Description = "PascalCase"
                Original = "PascalCase"
                Expected = Some { Slug = Slug "pascalcase"; Name = TagName "PascalCase" }
            }
            {
                Description = "camelCase"
                Original = "camelCase"
                Expected = Some { Slug = Slug "camelcase"; Name = TagName "camelCase" }
            }
            {
                Description = "snake_case"
                Original = "snake_case"
                Expected = Some { Slug = Slug "snake_case"; Name = TagName "snake_case" }
            }
            {
                Description = "dash-case"
                Original = "dash-case"
                Expected = Some { Slug = Slug "dash-case"; Name = TagName "dash-case" }
            }
            {
                Description = "Simple tag"
                Original = "tag"
                Expected = Some { Slug = Slug "tag"; Name = TagName "tag" }
            }
            {
                Description = "Tag with number"
                Original = "number-42"
                Expected = Some { Slug = Slug "number-42"; Name = TagName "number-42" }
            }
            {
                Description = "Only number"
                Original = "42"
                Expected = None
            }
            {
                Description = "Max length"
                Original = String.replicate 30 "a"
                Expected = Some { Slug = Slug "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"; Name = TagName "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" }
            }
            {
                Description = "Too long"
                Original = String.replicate 31 "a"
                Expected = None
            }
            {
                Description = "Too short"
                Original = "a"
                Expected = None
            }
            {
                Description = "Ok"
                Original = "Ok"
                Expected = Some { Slug = Slug "ok"; Name = TagName "Ok" }
            }
        ]

        for start in [ "#"; "-"; "_" ] do
            yield {
                Description = sprintf "Starts with - %A" start
                Original = sprintf "%s-tag" start
                Expected = None
            }

        for contains in [ "/"; "\\"; ":"; "@"; "$"; "~"; "!"; ","; "<"; ">"; "."; "("; ")"; "["; "]" ] do
            yield {
                Description = sprintf "With special char - %A" contains
                Original = sprintf "tag-%s" contains
                Expected = None
            }
    ]

[<Tests>]
let shouldParseLinks =
    testList "MF.EDC - Tag" [
        testCase "parse from string" <| fun _ ->
            provideTags
            |> List.iter (fun { Original = tagValue; Expected = expected; Description = description } ->
                let result = tagValue |> Tag.parse

                Expect.equal result expected description
            )
    ]
