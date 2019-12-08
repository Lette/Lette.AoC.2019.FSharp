module Day08
    open System
    open Common

    let xs =
        getInput 8
        |> List.head
        |> Seq.toList
        |> List.map (string >> int)

    let part1 () =

        xs
        |> List.chunkBySize (25 * 6)
        |> List.map (
            List.groupBy id
            >> List.map (fun (k, v) -> (k, List.length v))
        )
        |> List.sortBy (List.find (fst >> ((=) 0)) >> snd)
        |> List.head
        |> List.filter (fst >> ((<>) 0))
        |> List.map snd
        |> List.fold (*) 1

    let part2 () =

        let addPixels first second =
            match first, second with
            | 2, n -> n
            | n, _ -> n

        let addLayers xs ys =
            let folder x y state = addPixels x y :: state
            List.foldBack2 folder xs ys []

        let toOutputChar =
            function
            | 0 -> ' '
            | 1 -> '#'
            | _ -> failwith "all transparent???"

        printfn ""

        xs
        |> List.chunkBySize (25 * 6)
        |> List.reduce addLayers
        |> List.map toOutputChar
        |> List.chunkBySize 25
        |> List.map (List.toArray >> String)
        |> List.iter (printfn "%s") // --> "JCRCB"

        0

    let show () =
        showDay
            8
            part1 (Some 1474)
            part2 (Some 0)
