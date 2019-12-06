module Day05
    open Common
    open Computer

    let mem () =
        getInput 5
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map int

    let part1 () =

        runProgram (mem ()) [1] []
        |> finalOutput

    let part2 () =

        runProgram (mem ()) [5] []
        |> finalOutput

    let show () =
        showDay
            5
            part1 (Some 9025675)
            part2 (Some 11981754)
