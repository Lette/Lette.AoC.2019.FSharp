module Day11
    open System
    open Common
    open Computer

    let Black = 0I
    let White = 1I

    type Direction = Up | Right | Down | Left

    let turnRight =
        function
        | Up    -> Right
        | Right -> Down
        | Down  -> Left
        | Left  -> Up

    let turnLeft =
        function
        | Up    -> Left
        | Right -> Up
        | Down  -> Right
        | Left  -> Down

    let move (x, y) =
        function
        | Up    -> (x    , y + 1)
        | Right -> (x + 1, y    )
        | Down  -> (x    , y - 1)
        | Left  -> (x - 1, y    )

    let mem () =
        getInput 11
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map parseBigint

    let getColorAtPosition (x, y) hull =
        hull
        |> List.tryFind (fst >> (=) (x, y))
        |> function
            | None            -> Black
            | Some (_, color) -> color

    let runRobot firstInput robot =

        let rec run robot position direction nextInput hull =

            if isHalted robot then
                hull
            else
                let robot' = robot |> provideInput nextInput |> runProgram

                let (turn, color) =
                    match robot'.Output with
                    | t :: c :: _ -> (t, c)
                    | _ -> failwith "what? no output?"

                let hull' = (position, color) :: hull

                let direction' =
                    match turn with
                    | BigInt 0I -> turnLeft direction
                    | BigInt 1I -> turnRight direction
                    | _ -> failwith "unknown turn"

                let position' = move position direction'

                let nextInput' = getColorAtPosition position' hull'

                run robot' position' direction' nextInput' hull'

        run robot (0, 0) Up firstInput []

    let part1 () =

        createInitialState (mem ()) []
        |> expandMemory 2000
        |> runProgram
        |> runRobot 0I
        |> List.map fst
        |> List.distinct
        |> List.length

    let part2 () =

        let createRow xs =
            let xs' = xs |> List.sortBy (fst >> fst)

            let x =
                xs'
                |> List.head
                |> (fst >> fst)

            let folder s (_, color) = s + (if color = White then "#" else " ")
            let result = xs' |> List.fold folder ""

            (x, result)

        let padStrings rows =
            let padLeft (n, (s : string)) = s.PadLeft (n + s.Length)
            let minX =
                rows
                |> List.map fst
                |> List.min

            rows
            |> List.map (fun (x, s) -> (x - minX, s))
            |> List.map padLeft

        printfn ""

        createInitialState (mem ()) []
        |> expandMemory 2000
        |> runProgram
        |> runRobot 1I
        |> List.groupBy fst
        |> List.map (snd >> List.head)
        |> List.groupBy (fst >> snd)
        |> List.map (fun (y, xs) -> (y, createRow xs))
        |> List.sortByDescending fst
        |> List.map snd
        |> padStrings
        |> List.iter (printfn "%s")

        "PFKHECZU"  // <-- by manual ocr

    let show () =
        showDay
            11
            part1 (Some 1930)
            part2 (Some "PFKHECZU")
