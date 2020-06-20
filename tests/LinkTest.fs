module MF.EDC.LinkTest

open Expecto

type LinkTestCase = {
    Description: string
    Original: string
    Expected: Result<Link, LinkError>
}

let provideLinks =
    [
        {
            Description = "Empty link"
            Original = ""
            Expected = Error LinkError.Empty
        }
        {
            Description = "Not a link"
            Original = "not a link"
            Expected = Error (LinkError.IsNotWellFormed "not a link")
        }
        {
            Description = "Simple domain"
            Original = "www.domain.cz"
            Expected = Error (LinkError.IsNotWellFormed "www.domain.cz")
        }
        {
            Description = "Simple domain"
            Original = "https://www.domain.cz"
            Expected = Ok (Link "https://www.domain.cz/")
        }
        {
            Description = "Simple domain"
            Original = "http://domain.cz/"
            Expected = Ok (Link "http://domain.cz/")
        }
        {
            Description = "FTP connection"
            Original = "ftp://user:pass@www.domain.cz/"
            Expected = Ok (Link "ftp://user:pass@www.domain.cz/")
        }
        {
            Description = "Domain with query and hash"
            Original = "http://www.domain.com/product/?id=123#top"
            Expected = Ok (Link "http://www.domain.com/product/?id=123#top")
        }
        {
            Description = "Domain with query, hash and not allowed parameters"
            Original = "http://www.domain.com/product/?id=123&utm_hash=hash&utm_foo=bar&gclid=g123&fbid=fb321#top"
            Expected = Ok (Link "http://www.domain.com/product/?id=123#top")
        }
        {
            Description = "Url with port"
            Original = "http://www.special-domain.io:123/one/two/three"
            Expected = Ok (Link "http://www.special-domain.io:123/one/two/three")
        }
        {
            Description = "Correct url with max length"
            Original = "http://www.domain.com/product/?id=123#top" + String.replicate (500 - 41) "a"
            Expected = Ok (Link ("http://www.domain.com/product/?id=123#top" + String.replicate (500 - 41) "a"))
        }
        {
            Description = "Correct url with max length, with stripped parameters"
            Original = "http://www.domain.com/product/?id=123&utm_foo=bar#top" + String.replicate (500 - 41) "a"
            Expected = Ok (Link ("http://www.domain.com/product/?id=123#top" + String.replicate (500 - 41) "a"))
        }
        {
            Description = "Correct url but, too long"
            Original = "http://www.domain.com/product/?id=123#top" + String.replicate (500 - 41 + 1) "a"
            Expected = Error
                <| LinkError.TooLong (("http://www.domain.com/product/?id=123#top" + String.replicate (500 - 41 + 1) "a"), 500)
        }
    ]

[<Tests>]
let shouldParseLinks =
    testList "MF.EDC - Link" [
        testCase "parse from string" <| fun _ ->
            provideLinks
            |> List.iter (fun { Original = link; Expected = expected; Description = description } ->
                let result = link |> Link.parse

                Expect.equal result expected description
            )
    ]
