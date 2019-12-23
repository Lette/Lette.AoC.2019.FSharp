module Day15
    open System
    open Common
    open Computer

    type Block = Wall | Free | Oxygen | Unknown

    type P = P of (int * int)
    with
        static member origin = P (0, 0)

    type World = {
        Map : Block[,]
        Offset : P
        Oxygen : P option
    }
    with
        static member initial = {
            Map = Array2D.create 1 1 Free
            Offset = P.origin
            Oxygen = None
        }

    type Visualizer = {
        Print : (World -> unit)
        Clear : (unit -> unit)
    }

    type Direction = Up | Right | Down | Left

    let directions = [ Up; Right; Down; Left ]

    let mem () =
        getInput 15
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map parseBigint

    let toIntCodeResponse =
        function
        | Up -> 1I
        | Down -> 2I
        | Left -> 3I
        | Right -> 4I

    let toString =
        function
        | Wall -> "#"
        | Free -> " "
        | Oxygen -> "O"
        | Unknown -> "~"

    let toBlock =
        function
        | BigInt 0I -> Wall
        | BigInt 1I -> Free
        | BigInt 2I -> Oxygen
        | n  -> failwith (sprintf "unknown output: %A" n)

    let getPosition (P (x, y)) direction =
        match direction with
        | Up -> P (x, y - 1)
        | Down -> P (x, y + 1)
        | Left -> P (x - 1, y)
        | Right -> P (x + 1, y)

    let printToConsole { Map = map; Offset = P (minX, minY) } =

        consoleHome ()

        [0 .. Array2D.length2 map - 1]
        |> List.map (fun y -> map.[*, y] |> Array.map toString |> Array.toList |> List.reduce (+))
        |> List.iter (printfn "%s")

        printAt -minX -minY "+"
        System.Console.SetCursorPosition (0, 42)
        sleep 1

    let consoleVisualizer = { Print = printToConsole; Clear = consoleClear }
    let noVisualizer = { Print = (fun _ -> ()); Clear = (fun () -> ()) }
    let visualizer = noVisualizer

    let set (P (x, y)) block ({ Map = map; Offset = P (minX, minY) } as world) =

        let a' = Array2D.expandSet Unknown block (x - minX) (y - minY) map

        let minX' = if x < minX then minX - 1 else minX
        let minY' = if y < minY then minY - 1 else minY

        { world with
            Map = a'
            Offset = P (minX', minY')
        }

    let get (P (x, y)) { Map = map; Offset = P (minX, minY) } =

        Array2D.expandGet Unknown (x - minX) (y - minY) map

    let createWorld () =

        visualizer.Clear ()

        let checkDirection (position, computer) world direction =
            let position' = getPosition position direction

            match get position' world with
            | Unknown ->

                let direction' = toIntCodeResponse direction

                let (block', computer') =
                    computer
                    |> clone
                    |> provideInput direction'
                    |> runProgram
                    |> popOutput
                    |> Tuple2.mapFst (List.head >> toBlock)

                match block' with
                | Unknown -> failwith "can't happen!"
                | Wall    -> (None,                        world |> set position' Wall)
                | Free    -> (Some (position', computer'), world |> set position' Free)
                | Oxygen  ->
                    (
                        Some (position', computer'),
                        { (world |> set position' Oxygen) with
                            Oxygen = Some position'
                        }
                    )

            | _ -> (None, world)

        let rec run states world =
            visualizer.Print world

            match states with
            | [] -> world
            | state :: ss ->
                let (newStates, world') =
                    directions
                    |> List.mapFold (checkDirection state) world
                    |> Tuple2.mapFst (List.choose id)
                run (ss @ newStates) world'

        run [(P.origin, createInitialState (mem ()) [])] World.initial

    let createSteps world =
        let { Map = map; Offset = P (minX, minY) } = world
        let steps = Array2D.create (Array2D.length1 map) (Array2D.length2 map) -1

        let setStep (P (x, y)) step =
            Array2D.set steps (x - minX) (y - minY) step

        let getStep (P (x, y)) =
            Array2D.get steps (x - minX) (y - minY)

        let checkDirection position step direction =
            let p' = getPosition position direction
            let step' = getStep p'
            let block = get p' world

            match step', block with
            | -1  , Free ->
                setStep p' (step + 1)
                set p' Oxygen world |> ignore
                Some p'
            | _ -> None

        let rec run cells =
            visualizer.Print world
            match cells with
            | [] -> ()
            | cell :: cs ->
                let cells =
                    directions
                    |> List.choose (checkDirection cell (getStep cell))

                run (cs @ cells)

        setStep world.Oxygen.Value 0
        run [world.Oxygen.Value]
        steps

    let world = lazy ( createWorld () )
    let steps = lazy ( createSteps world.Value )

    let part1 () =

        let (P (minX, minY)) = world.Value.Offset
        Array2D.get steps.Value -minX -minY

    let part2 () =

        steps.Value
        |> Array2D.toSeq
        |> Seq.max

    let show () =
        showDay
            15
            part1 (Some 246)
            part2 (Some 376)
