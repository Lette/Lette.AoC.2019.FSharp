module Day13
    open System
    open Common
    open Computer

    let Block = 2I
    let Paddle = 3I
    let Ball = 4I
    let Score = -1I

    let mem () =
        getInput 13
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map parseBigint

    let part1 () =
        createInitialState (mem ()) []
        |> expandMemory 3000
        |> runProgram
        |> fun s -> s.Output
        |> List.chunkBySize 3
        |> List.filter (List.head >> ((=) Block))
        |> List.length

    let parseOutput output =
        
        let rec run output ((p, b, s) as acc) =
            match output with
            | BigInt Paddle :: _ :: x            :: os -> run os (Some x, b, s)
            | BigInt Ball   :: _ :: x            :: os -> run os (p, Some x, s)
            | n             :: _ :: BigInt Score :: os -> run os (p, b, Some n)
            | _             :: _ :: _            :: os -> run os (p, b, s)
            | [] -> acc
            | _  -> failwith "not an expected sequence of outputs!"

        run output (None, None, None)

    let part2 () =

        let rec run (paddle, ball, score) state =

            if isHalted state then
                score
            else
                state
                |> provideInput ((ball - paddle) |> sign |> bigint)
                |> runProgram
                |> popOutput
                |> Tuple2.mapFst parseOutput
                |> Tuple2.mapFst (Tuple3.defaultValueEach (paddle, ball, score))
                ||> run

        createInitialState (mem ()) []
        |> setMemory 0 2I
        |> expandMemory 3000
        |> run (0I, 0I, 0I)

    let show () =
        showDay
            13
            part1 (Some 242)
            part2 (Some 11641I)
