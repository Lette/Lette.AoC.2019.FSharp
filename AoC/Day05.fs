module Day05
    open Common
    open Computer

    let mem () =
        getInput 5
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map int

    let part1 () =

        createInitialState (mem ()) [1]
        |> runProgram
        |> finalOutput

    let part2 () =

        createInitialState (mem ()) [5]
        |> runProgram
        |> finalOutput

    let show () =
        showDay
            5
            part1 (Some 9025675)
            part2 (Some 11981754)
