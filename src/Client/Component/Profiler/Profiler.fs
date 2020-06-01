[<RequireQualifiedAccess>]
module Profiler

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fulma
open Fulma.Extensions.Wikiki
open Thoth.Json

open Shared
open ProfilerModel

let private statusColor = function
    | Some Profiler.Yellow -> "sf-toolbar-status sf-toolbar-status-yellow"
    | Some Profiler.Green -> "sf-toolbar-status sf-toolbar-status-green"
    | Some Profiler.Red -> "sf-toolbar-status sf-toolbar-status-red"
    | Some Profiler.Gray -> "sf-toolbar-status sf-toolbar-status-norma"
    | _ -> ""

let private tooltipColor = function
    | Some Profiler.Yellow -> Tooltip.IsInfo
    | Some Profiler.Green -> Tooltip.IsSuccess
    | Some Profiler.Red -> Tooltip.IsDanger
    | _ -> ""

let private infoGroupPiece ({ ShortLabel = shortLabel; Label = (Profiler.Label label); Value = (Profiler.Value value); Detail = detail; Color = color; Link = link }: Profiler.DetailItem) =
    div [ Class "sf-toolbar-info-piece" ] [
        let shortLabelValue =
            match shortLabel with
            | Some (Profiler.Label shortLabel) -> Some shortLabel
            | _ -> None

        let link label =
            match link with
            | Some (Profiler.Link link) -> a [ Href link ] [ str label ]
            | _ -> str label

        match shortLabelValue with
        | Some shortLabel ->
            b [
                Tooltip.dataTooltip label
                Class (className [
                    Tooltip.ClassName
                    Tooltip.IsTooltipTop
                    Tooltip.IsMultiline
                    color |> tooltipColor
                ])
            ] [ link shortLabel ]
        | _ -> b [] [ link label ]

        let valueSpan =
            match detail with
            | Some (Profiler.ValueDetail detail) ->
                span [
                    Tooltip.dataTooltip detail
                    Class (className [
                        Tooltip.ClassName
                        Tooltip.IsTooltipTop
                        Tooltip.IsMultiline
                        color |> statusColor
                    ])
                ]
            | _ -> span [ Class (color |> statusColor) ]

        valueSpan [ str value ]
    ]

let private infoGroup values =
    div [ Class "sf-toolbar-info" ] [
        values
        |> List.map infoGroupPiece
        |> div [ Class "sf-toolbar-info-group" ]
    ]

let private itemStatus ({ Color = color; Value = (Profiler.Value value) }: Profiler.Status) =
    span [ Class (color |> statusColor) ] [ str value ]

let private itemLabel (Profiler.Label label) =
    span [ Class "sf-toolbar-label" ] [ str label ]

let private itemUnit (Profiler.Unit unit) =
    span [ Class "sf-toolbar-label" ] [ str unit ]

let private itemValue (Profiler.Value value) =
    span [ Class "sf-toolbar-value" ] [ str value ]

let private itemNormal (item: Profiler.Item) =
    div [ Class (sprintf "sf-toolbar-block %s" (item.ItemColor |> statusColor)) ] [
        a [] [
            div [ Class "sf-toolbar-icon" ] [
                yield!
                    match item.StatusIcon with
                    | Some status ->
                        [
                            itemStatus status
                            str " "
                        ]
                    | _ -> []

                yield!
                    match item.Label with
                    | Some label ->
                        [
                            itemLabel label
                            str " "
                        ]
                    | _ -> []

                itemValue item.Value

                yield!
                    match item.Unit with
                    | Some unit ->
                        [
                            str " "
                            itemUnit unit
                        ]
                    | _ -> []
            ]
        ]

        infoGroup item.Detail
    ]

let profiler refreshProfiler ({ Profiler = profiler }: ProfilerModel) dispatch =
    match profiler with
    | Some (Profiler.Toolbar profiler) ->
        refreshProfiler()

        div [ Class "sf-toolbar" ] [
            profiler
            |> List.map itemNormal
            |> div [ Class "sf-toolbarreset clear-fix" ]
        ]
    | _ -> fragment [] []
