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

    let getPosition (x, y) d =
        match d with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    let printToConsole { Map = map; Offset = P (minX, minY) } =

        consoleHome ()

        [0 .. Array2D.length2 map - 1]
        |> List.map (fun y -> map.[*, y] |> Array.map toString |> Array.toList |> List.reduce (+))
        |> List.iter (printfn "%s")

        printAt -minX -minY "+"
        System.Console.SetCursorPosition (0, 42)
        sleep 1

    let consoleVisualizer = { Print = printToConsole; Clear = System.Console.Clear }
    let noVisualizer = { Print = (fun _ -> ()); Clear = (fun () -> ()) }

    let set (x, y) block ({ Map = map; Offset = P (minX, minY) } as world) =

        let a' = Array2D.expandSet Unknown block (x - minX) (y - minY) map

        let minX' = if x < minX then minX - 1 else minX
        let minY' = if y < minY then minY - 1 else minY

        { world with
            Map = a'
            Offset = P (minX', minY')
        }

    let get (x, y) { Map = map; Offset = P (minX, minY) } =

        Array2D.expandGet Unknown (x - minX) (y - minY) map

    let part1 () =
        let visualizers = (noVisualizer, consoleVisualizer)
        let visualizer = snd visualizers
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
                            Oxygen = Some (P position')
                        }
                    )

            | _ -> (None, world)

        let createWorld () =

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

            run [((0, 0), createInitialState (mem ()) [])] World.initial

        let world = createWorld ()

        246   // by manual count!

    let part2 () =
        376   // by manual count!

    let show () =
        showDay
            15
            part1 (Some 246)
            part2 (Some 376)
