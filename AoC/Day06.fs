module Day06
    open Common

    let toOrbit =
        function
        | Regex "([A-Z0-9]+)\)([A-Z0-9]+)" [ a; b ] -> (a, b)
        | s -> failwith (sprintf "unrecognized orbit: %s" s)

    let orbits =
        getInput 6
        |> List.map toOrbit

    let part1 () =

        let rec runOrbits parents children weight acc =

            match parents, children with
            | []     , [] -> acc
            | []     , _  -> runOrbits children [] (weight + 1) acc
            | p :: ps, _  ->
                let newChildren =
                    orbits
                    |> List.filter (fst >> (=) p)
                    |> List.map snd
                runOrbits ps (newChildren @ children) weight (acc + weight * (List.length newChildren))
 
        runOrbits ["COM"] [] 1 0

    let part2 () =

        let findParent child =
            orbits
            |> List.find (snd >> (=) child)
            |> fst

        let rec stripEqualElementsFromStart a b =
            match a, b with
            | [], _                        -> (a, b)
            | _ , []                       -> (a, b)
            | x :: _ , y :: _  when x <> y -> (a, b)
            | _ :: xs, _ :: ys             -> stripEqualElementsFromStart xs ys

        let findFullPath child =

            let rec run children =
                match children with
                | "COM" :: _ -> children
                | child :: _ -> run (findParent child :: children)
                | []         -> failwith "how is this empty?"

            run [child]

        (findFullPath "YOU", findFullPath "SAN")
        ||> stripEqualElementsFromStart
        ||> fun a b -> List.length a + List.length b - 2

    let show () =
        showDay
            6
            part1 (Some 227612)
            part2 (Some 454)
