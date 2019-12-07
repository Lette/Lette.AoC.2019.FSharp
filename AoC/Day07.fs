module Day07
    open Common
    open Computer

    let mem () =
        getInput 7
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map int

    let part1 () =

        let runAmplifier phaseSetting input =
            createInitialState (mem ()) [ phaseSetting; input ]
            |> runProgram
            |> finalOutput

        let runAmplifiers phaseSettings =
            let rec run phases input =
                match phases with
                | []      -> input
                | p :: ps -> run ps (runAmplifier p input) 

            run phaseSettings 0

        permutations [ 0 .. 4 ]
        |> List.map runAmplifiers
        |> List.max

    let part2 () =


        let initAmplifiers phaseSettings =

            let initAmplifier phaseSetting =
                createInitialState (mem ()) [ phaseSetting ]
                |> runProgram

            let rec run phases acc =
                match phases with
                | []      -> List.rev acc
                | p :: ps -> run ps ((initAmplifier p) :: acc) 

            run phaseSettings []

        let runAmplifiers amps =

            let rec run amps input =

                let rec runOne amps input acc =
                    match amps with
                    | []      -> (input, List.rev acc)
                    | x :: xs ->
                        let newState =
                            if isWaiting x then
                                x |> provideInput input |> runProgram
                            else
                                failwith "not waiting!"
                        let lastOutput = newState |> lastOutput

                        runOne xs lastOutput (newState :: acc)

                let (output, newAmps) = runOne amps input []

                if newAmps |> List.exists isHalted then
                    output
                else
                    run newAmps output

            run amps 0

        permutations [ 5 .. 9 ]
        |> List.map initAmplifiers
        |> List.map runAmplifiers
        |> List.max

    let show () =
        showDay
            7
            part1 (Some 45730)
            part2 (Some 5406484)
