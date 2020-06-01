namespace MF.EDC.Profiler

[<RequireQualifiedAccess>]
module Profiler =
    open System
    open MF.EDC

    open Queries
    open Errors

    open Shared

    let token = Profiler.Token "...todo - read on startup..."

    let private queryItem color query: Profiler.DetailItem =
        let { Target = (Target (method, Url target)); Created = created } = query.Target

        let shortUrl =
            (target.TrimEnd '/').Split '/'
            |> List.ofArray
            |> List.rev
            |> List.head
            |> (+) "/"

        {
            ShortLabel = Some (Profiler.Label (sprintf "[%A] %s" method shortUrl))
            Label = Profiler.Label target
            Detail = Some (Profiler.ValueDetail query.Response)
            Value = Profiler.Value (created.ToString("HH:mm:ss yyyy-MM-dd"))
            Color = Some color
            Link = Some (Profiler.Link target)
        }

    let private errorItem (ErrorMessage message, (created: DateTime)): Profiler.DetailItem =
        let shortLabel =
            if message.Length > 50
                then message.Substring(0, 50) + " ..."
                else message

        {
            ShortLabel = Some (Profiler.Label shortLabel)
            Label = Profiler.Label message
            Detail = None
            Value = Profiler.Value (created.ToString("HH:mm:ss yyyy-MM-dd"))
            Color = Some Profiler.Red
            Link = None
        }

    let init currentApplication debug =
        let errorsCount = Errors.count()

        Profiler.Toolbar [
            yield {
                Id = Profiler.Id "Application"
                Label = None
                Value = Profiler.Value (currentApplication |> Instance.value)
                Unit = None
                ItemColor = Some Profiler.Green
                StatusIcon = None
                Detail = [
                    Profiler.Detail.createItem (Profiler.Label "Debug") (Profiler.Value debug) |> Profiler.Detail.addColor (if debug.Contains "Dev" then Profiler.Color.Yellow else Profiler.Color.Green)
                    Profiler.Detail.createItem (Profiler.Label "Assembly Version") (Profiler.Value AssemblyVersionInformation.AssemblyVersion)
                    Profiler.Detail.createItem (Profiler.Label "SafeTemplate Version") (Profiler.Value AssemblyVersionInformation.AssemblyMetadata_SafeTemplateVersion)
                    Profiler.Detail.createItem (Profiler.Label "Build Number") (Profiler.Value AssemblyVersionInformation.AssemblyMetadata_buildNumber)
                    Profiler.Detail.createItem (Profiler.Label "Builded At") (Profiler.Value AssemblyVersionInformation.AssemblyMetadata_createdAt)
                ]
            }

            yield {
                Id = Profiler.Id "Git"
                Label = Some (Profiler.Label "Git")
                Value = Profiler.Value AssemblyVersionInformation.AssemblyMetadata_gitbranch
                Unit = None
                ItemColor = None
                StatusIcon = None
                Detail = [
                    Profiler.Detail.createItem (Profiler.Label "Git Branch") (Profiler.Value AssemblyVersionInformation.AssemblyMetadata_gitbranch)
                    Profiler.Detail.createItem (Profiler.Label "Git Commit") (Profiler.Value AssemblyVersionInformation.AssemblyMetadata_gitcommit)
                ]
            }

            yield {
                Id = Profiler.Id "Queries"
                Label = None
                Value = Profiler.Value (Queries.count() |> string)
                Unit = Some (Profiler.Unit "Queries")
                ItemColor = None
                StatusIcon = None
                Detail =
                    Queries.values ()
                    |> List.takeUpTo 10
                    |> List.map (function
                        | Query (Ok queryData) -> queryData |> queryItem Profiler.Green
                        | Query (Error queryData) -> queryData |> queryItem Profiler.Red
                    )
            }

            if errorsCount > 0 then
                yield {
                    Id = Profiler.Id "Errors"
                    Label = None
                    Value = Profiler.Value (errorsCount |> string)
                    Unit = Some (Profiler.Unit "Errors")
                    ItemColor = Some (Profiler.Red)
                    StatusIcon = None
                    Detail =
                        Errors.values ()
                        |> List.takeUpTo 10
                        |> List.map errorItem
                }
        ]
