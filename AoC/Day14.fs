module Day14
    open System
    open Common

    let toChem (s : string) =
        match (s.Trim ()).Split " " with
        | [| q; c |] -> (int q, c)
        | _ -> failwith (sprintf "what chem is %s?" s)

    let toChemicals (s : string) =
        let rec run chems acc =
            match chems with
            | [] -> acc
            | c :: cs -> run cs (toChem c :: acc)

        run (s.Split ',' |> Array.toList) []

    let toReaction (s : string) =
        match s.Split "=>" with
        | [| inputs; output |] -> (toChemicals inputs, toChem output)
        | _ -> failwith "can't understand!"

    let toMaps (inputs, output) =
        let rec run inputs acc =
            match inputs with
            | [] -> acc
            | (q, c) :: is -> run is (((q, c), output) :: acc)
        run inputs []

    let xs () =
        getInput 14
        |> List.map toReaction
        |> List.collect toMaps

    let findMaps chem maps =
        maps |> List.partition (fun (_, (_, oc)) -> oc = chem)

    let getQ cq iq oq = (float cq) / (float oq) |> ceil |> int |> (*) iq

    let findLeaf cmap outputs =
        let l =
            outputs
            |> List.find (fun (_, c) -> cmap |> List.forall (fun ((_, ic), _) -> ic <> c))
        (l, List.except [l] outputs)

    let normalizeOutputs outputs =
        outputs
        |> List.groupBy (fun (_, c) -> c)
        |> List.map (fun (k, vs) -> (vs |> List.sumBy fst, k))

    let part1 () =

        let rec run maps outputs =
            match outputs with
            | [] -> failwith "that's unexpected!"
            | [(n, "ORE")] -> n
            | _ ->
                let ((cq, cc), outputs') = findLeaf maps outputs
                let (mapsToLeaf, maps') = findMaps cc maps
                let newOutputs =
                    mapsToLeaf
                    |> List.map (fun ((iq, ic), (oq, _)) -> (getQ cq iq oq, ic))

                outputs' @ newOutputs
                |> normalizeOutputs
                |> run maps'

        run (xs ()) [(1, "FUEL")]

    let part2 () =
        0

    let show () =
        showDay
            14
            part1 (Some 873899)
            part2 None
