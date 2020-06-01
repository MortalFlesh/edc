[<AutoOpen>]
module Types

open Shared

type AsyncStatus =
    | Inactive
    | InProgress
    | Completed

type InputState =
    | Neutral
    | Success
    | WithError of ErrorMessage
