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
        
        let rec runOrbits leaves newLeaves weight acc =
            
            match leaves, newLeaves with
            | [], []     -> acc
            | [], _      -> runOrbits newLeaves [] (weight + 1) acc
            | l :: ls, _ ->
                let subOrbits =
                    orbits
                    |> List.filter (fst >> ((=) l))
                    |> List.map snd
                runOrbits ls (subOrbits @ newLeaves) weight (acc + weight * (List.length subOrbits))
 
        runOrbits ["COM"] [] 1 0

    let part2 () =
        0

    let show () =
        showDay
            6
            part1 (Some 227612)
            part2 None
