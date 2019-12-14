module Day14
    open System
    open Common

    let toChem (s : string) =
        match (s.Trim ()).Split " " with
        | [| q; c |] -> (parseBigint q, c)
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

    let getRequiredQuantity cq iq oq =
        let q = cq / oq
        let r = cq % oq
        if r = 0I then
            q * iq
        else
            (q + 1I) * iq

    let findLeaf cmap outputs =
        let l =
            outputs
            |> List.find (fun (_, c) -> cmap |> List.forall (fun ((_, ic), _) -> ic <> c))
        (l, List.except [l] outputs)

    let normalizeOutputs outputs =
        outputs
        |> List.groupBy (fun (_, c) -> c)
        |> List.map (fun (k, vs) -> (vs |> List.sumBy fst, k))

    let rec run maps outputs =
        match outputs with
        | [] -> failwith "that's unexpected!"
        | [(n, "ORE")] -> n
        | _ ->
            let ((cq, cc), outputs') = findLeaf maps outputs
            let (mapsToLeaf, maps') = findMaps cc maps
            let newOutputs =
                mapsToLeaf
                |> List.map (fun ((iq, ic), (oq, _)) -> (getRequiredQuantity cq iq oq, ic))

            outputs' @ newOutputs
            |> normalizeOutputs
            |> run maps'

    let part1 () =

        run (xs ()) [(1I, "FUEL")]

    let part2 () =

        let tryRun n = run (xs ()) [(n, "FUEL")]

        let range = (1000000I, 2000000I)

        let rec bisect range =
            match range with
            | (a, b) when a + 1I = b -> a
            | (a, b) ->
                let c = (a + b) / 2I
                let o = tryRun c
                if o < 1000000000000I then
                    bisect (c, b)
                else
                    bisect (a, c)

        bisect range

    let show () =
        showDay
            14
            part1 (Some 873899I)
            part2 (Some 1893569I)
