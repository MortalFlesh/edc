module Validations

type ValidationErrors = Map<string, string list>
type Validation<'Success> = Result<'Success, ValidationErrors>

/// Functions for the `Validation` type (mostly applicative)
[<RequireQualifiedAccess>]  // RequireQualifiedAccess forces the `Validation.xxx` prefix to be used
module Validation =
    let private merge (errors1: ValidationErrors) (errors2: ValidationErrors): ValidationErrors =
        errors1
        |> Map.fold (fun merged field fieldErrors2 ->
            match merged |> Map.tryFind field with
            | Some fieldErrors1 -> merged.Add(field, fieldErrors1 @ fieldErrors2 |> List.distinct)
            | _ -> merged.Add(field, fieldErrors2)
        ) errors2

    /// Apply a Validation<fn> to a Validation<x> applicatively
    let apply (fV:Validation<_>) (xV:Validation<_>) :Validation<_> =
        match fV, xV with
        | Ok f, Ok x -> Ok (f x)
        | Error errors1, Ok _ -> Error errors1
        | Ok _, Error errors2 -> Error errors2
        | Error errors1, Error errors2 -> Error (merge errors1 errors2)

    // combine a list of Validation, applicatively
    let sequence (aListOfValidations:Validation<_> list) =
        let (<*>) = apply
        let (<!>) = Result.map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfValidations initialValue

    // combine a list of Validation, applicatively and fold success results into one
    let sequenceFold f aListOfValidations =
        aListOfValidations
        |> sequence
        |> Result.map f
