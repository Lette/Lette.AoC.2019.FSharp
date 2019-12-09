module Day03
    open System
    open Common

    type Direction = Up | Right | Down | Left
    type Segment = Segment of Direction : Direction * Distance : int
    type Wire = Wire of Id : int * Segments : Segment list

    let toDirection =
        function
        | "U" -> Up
        | "R" -> Right
        | "D" -> Down
        | "L" -> Left
        | s   -> failwith (sprintf "could not parse %s" s)

    let partPattern = @"(\w)(\d+)"

    let toSegment =
        function
        | Regex partPattern [direction; distance] -> Segment (toDirection direction, int distance)
        | s -> failwith (sprintf "could not parse %s" s)

    let toSegments (s : string) =
        s.Split ','
        |> Array.toList
        |> List.map toSegment

    let toWire i s =
        Wire (i, toSegments s)

    let xs =
        lazy (
            getInput 3
            |> List.mapi toWire
        )

    let runWire (Wire (id, segments)) =

        let rec run segments acc =
            let (x, y, step) = List.head acc
            match segments with
            | [] -> acc
            | Segment (_,     0) :: ps -> run                              ps                               acc
            | Segment (Up,    n) :: ps -> run (Segment (Up,    (n - 1)) :: ps) ((x,     y + 1, step + 1) :: acc)
            | Segment (Right, n) :: ps -> run (Segment (Right, (n - 1)) :: ps) ((x + 1, y    , step + 1) :: acc)
            | Segment (Down,  n) :: ps -> run (Segment (Down,  (n - 1)) :: ps) ((x,     y - 1, step + 1) :: acc)
            | Segment (Left,  n) :: ps -> run (Segment (Left,  (n - 1)) :: ps) ((x - 1, y    , step + 1) :: acc)

        let positions = run segments [(0, 0, 0)]
        (id, positions)

    let manhattanDistance (x, y) = abs x + abs y

    let folder map (id, positions) =

        let innerFolder map (x, y, s) =
            match Map.tryFind (x, y) map with
            | None   -> Map.add (x, y) (Map.add id s Map.empty) map
            | Some m ->
                match Map.tryFind id m with
                | None              -> Map.add (x, y) (Map.add id s m) map
                | Some t when t > s -> Map.add (x, y) (Map.add id s m) map
                | _                 -> map

        positions |> List.fold innerFolder map

    let crossings =
        lazy (
            xs.Value
            |> List.map runWire
            |> List.fold folder Map.empty
            |> Map.remove (0, 0)
            |> Map.toList
            |> List.filter (fun (_, n) -> Map.count n > 1)
        )

    let part1 () =

        crossings.Value
            |> List.map (fst >> manhattanDistance)
            |> List.min

    let part2 () =

        crossings.Value
            |> List.map snd
            |> List.map (
                    Map.toList
                    >> List.map snd
                    >> List.sum
                )
            |> List.min

    let show () =
        showDay
            3
            part1 (Some 806)
            part2 (Some 66076)
