namespace State

module ConcurrentStorage =
    open System.Collections.Concurrent

    type Key<'UniqueData> = Key of 'UniqueData

    [<RequireQualifiedAccess>]
    module Key =
        let value (Key key) = key

    type private Storage<'UniqueData, 'State> = ConcurrentDictionary<Key<'UniqueData>, 'State>
    type State<'UniqueData, 'State> = private State of Storage<'UniqueData, 'State>

    let private create<'UniqueData, 'State> () =
        Storage<'UniqueData, 'State>()
        |> State

    let private setState (State storage) key state =
        storage.AddOrUpdate(key, state, fun _ _ -> state)
        |> ignore

    let private addOrUpdateState (State storage) (update: Key<'a> -> 'b -> 'b -> 'b) key newState =
        storage.AddOrUpdate(key, newState, (fun key currentState -> update key currentState newState))
        |> ignore

    let private getState (State storage) key =
        match storage.TryGetValue key with
        | true, state -> Some state
        | _ -> None

    let private getAllKeys (State storage) =
        storage.Keys

    let private getAllValues (State storage) =
        storage.Values

    let private countAll (State storage) =
        storage.Count

    [<RequireQualifiedAccess>]
    module State =
        let empty = create

        let iter f (State storage) =
            storage
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> Seq.iter f

        let length = countAll

        let tryFind key storage =
            key |> getState storage

        let set key value storage =
            value |> setState storage key

        let update key value update storage =
            value |> addOrUpdateState storage update key

        let keys storage =
            storage
            |> getAllKeys
            |> List.ofSeq

        let values storage =
            storage
            |> getAllValues
            |> List.ofSeq

        let items storage =
            storage
            |> values
            |> List.zip (storage |> keys)

        let tryRemove key (State storage) =
            storage.TryRemove(key)
            |> ignore

        let clear (State storage) =
            storage.Clear()

        let keepLastSortedBy toKeep f state =
            if toKeep <= 0 then empty()
            else
                match state |> length with
                | underLimit when underLimit <= toKeep -> state
                | overLimit ->
                    let toDelete = overLimit - toKeep

                    if toDelete >= toKeep
                        then // (to delete >= to keep) -> just copy those to keep (create new)
                            let newState = empty()

                            state
                            |> items
                            |> Seq.sortByDescending f
                            |> Seq.take toKeep
                            |> Seq.iter (fun (k, v) -> newState |> set k v)

                            newState

                        else // (to delete < to keep)  -> just remove those to delete
                            state
                            |> items
                            |> Seq.sortBy f
                            |> Seq.take toDelete
                            |> Seq.iter (fun (k, _) -> state |> tryRemove k)

                            state
