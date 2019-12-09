module Day09
    open System
    open Common
    open Computer

    let mem () =
        getInput 9
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map parseBigint

    let part1 () =
        createInitialState (mem()) [ 1I ]
        |> expandMemory 2000
        |> runProgram
        |> finalOutput

    let part2 () =

        createInitialState (mem()) [ 2I ]
        |> expandMemory 2000
        |> runProgram
        |> finalOutput

    let show () =
        showDay
            9
            part1 (Some 2932210790I)
            part2 (Some 73144I)
