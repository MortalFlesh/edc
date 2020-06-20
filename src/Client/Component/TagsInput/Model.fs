module TagsInputModule

open Elmish

open Shared
open Shared.Dto.Common

type TagsInputModel = {
    NewTag: string
    Tags: Result<Tag, WrongTag> list
    Validating: AsyncStatus
    Error: ErrorMessage option
}

[<RequireQualifiedAccess>]
type TagsInputAction =
    | ChangeValue of string

    | ValidateTag of string
    | TagIsValid of Tag
    | TagIsInvalid of string * ErrorMessage

    | DeleteTag of string

type DispatchTagsInputAction = TagsInputAction -> unit

[<RequireQualifiedAccess>]
module TagsInputModel =
    let empty = {
        NewTag = ""
        Tags = []
        Validating = Inactive
        Error = None
    }

    let update<'ParentAction> (liftAction: TagsInputAction -> 'ParentAction) (model: TagsInputModel): TagsInputAction -> TagsInputModel * Cmd<'ParentAction> = function
        | TagsInputAction.ChangeValue value ->
            match model with
            | { Validating = InProgress } -> model, Cmd.none
            | _ when [ " "; ";"; "," ] |> List.exists value.EndsWith ->
                { model with Validating = InProgress; Error = None },
                value.Trim([| ' '; ','; ';'|])
                |> TagsInputAction.ValidateTag
                |> Cmd.ofMsg
                |> Cmd.map liftAction
            | _ ->
                { model with NewTag = value; Error = None }, Cmd.none

        | TagsInputAction.ValidateTag value ->
            model,
            Cmd.OfAsyncImmediate.result (
                value
                |> Api.validateTag
                    TagsInputAction.TagIsValid
                    (fun e -> TagsInputAction.TagIsInvalid (value, e))
            )
            |> Cmd.map liftAction

        | TagsInputAction.TagIsValid tag ->
            { model with
                Validating = Inactive
                NewTag = ""
                Tags = model.Tags @ [ Ok tag ] |> List.distinct
            }, Cmd.none

        | TagsInputAction.TagIsInvalid (invalidTag, error) ->
            { model with
                Validating = Inactive
                NewTag = ""
                Tags = model.Tags @ [ Error (WrongTag invalidTag) ] |> List.distinct
                Error = Some error
            }, Cmd.none

        | TagsInputAction.DeleteTag tagToDelete ->
            { model with
                Error = None
                Tags =
                    model.Tags
                    |> List.filter (function
                        | Ok tag -> tag |> Tag.value <> tagToDelete
                        | Error (WrongTag tag) -> tag <> tagToDelete
                    )
            }, Cmd.none
