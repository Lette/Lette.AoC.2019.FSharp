module Day07
    open Common
    open Computer

    let mem () =
        getInput 7
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map int

    let part1 () =

        let runAmplifier input phaseSetting =
            createInitialState (mem ()) [ phaseSetting; input ]
            |> runProgram
            |> finalOutput

        permutations [ 0 .. 4 ]
        |> List.map (List.fold runAmplifier 0)
        |> List.max

    let part2 () =

        let initAmplifier phaseSetting =
            createInitialState (mem ()) [ phaseSetting ]
            |> runProgram

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
                        let output = newState |> lastOutput

                        runOne xs output (newState :: acc)

                let (output, newAmps) = runOne amps input []

                if newAmps |> List.exists isHalted then
                    output
                else
                    run newAmps output

            run amps 0

        permutations [ 5 .. 9 ]
        |> List.map (List.map initAmplifier)
        |> List.map runAmplifiers
        |> List.max

    let show () =
        showDay
            7
            part1 (Some 45730)
            part2 (Some 5406484)
