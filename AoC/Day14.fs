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

    let xs () =
        getInput 14
        |> List.map toReaction

    let part1 () =
        0

    let part2 () =
        0

    let show () =
        showDay
            14
            part1 None
            part2 None
