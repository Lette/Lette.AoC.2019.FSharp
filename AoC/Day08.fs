module Day08
    open System
    open Common

    type Color = Black | White | Transparent

    let toColor =
        function
        | '0' -> Black
        | '1' -> White
        | '2' -> Transparent
        | c   -> failwith (sprintf "what's %c?" c)

    let xs =
        getInput 8
        |> List.head
        |> Seq.toList
        |> List.map toColor

    let part1 () =

        xs
        |> List.chunkBySize (25 * 6)
        |> List.map (
            List.groupBy id
            >> List.map (fun (k, vs) -> (k, List.length vs))
        )
        |> List.sortBy (List.find (fst >> ((=) Black)) >> snd)
        |> List.head
        |> List.filter (fst >> ((<>) Black))
        |> List.map snd
        |> List.fold (*) 1

    let part2 () =

        let addPixels first second =
            match first, second with
            | Transparent, n -> n
            | n          , _ -> n

        let addLayers xs ys =
            let folder x y state = addPixels x y :: state
            List.foldBack2 folder xs ys []

        let toOutputChar =
            function
            | Black       -> ' '
            | White       -> '#'
            | Transparent -> failwith "all transparent???"

        printfn ""

        xs
        |> List.chunkBySize (25 * 6)
        |> List.reduce addLayers
        |> List.map toOutputChar
        |> List.chunkBySize 25
        |> List.map (List.toArray >> String)
        |> List.iter (printfn "%s")

        "JCRCB" // <-- by manual OCR!

    let show () =
        showDay
            8
            part1 (Some 1474)
            part2 (Some "JCRCB")
