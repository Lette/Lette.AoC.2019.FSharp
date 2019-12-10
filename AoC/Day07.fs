module Day07
    open Common
    open Computer

    let mem () =
        getInput 7
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map parseBigint

    let part1 () =

        let runAmplifier input phaseSetting =
            createInitialState (mem ()) [ phaseSetting; input ]
            |> runProgram
            |> finalOutput

        permutations [ 0I .. 4I ]
        |> List.map (List.fold runAmplifier 0I)
        |> List.max

    let part2 () =

        let initAmplifier phaseSetting =
            createInitialState (mem ()) [ phaseSetting ]
            |> runProgram

        let runAmplifiers amps =

            let rec run amps input =

                if amps |> List.exists isHalted then
                    input

                else
                    let runAmp input amp =
                        let newState =
                            amp
                            |> provideInput input
                            |> runProgram

                        let output = newState |> lastOutput

                        (newState, output)

                    amps
                    |> List.mapFold runAmp input
                    ||> run

            run amps 0I

        permutations [ 5I .. 9I ]
        |> List.map (List.map initAmplifier)
        |> List.map runAmplifiers
        |> List.max

    let show () =
        showDay
            7
            part1 (Some 45730I)
            part2 (Some 5406484I)
