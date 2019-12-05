module Day05
    open System
    open Common
    open Computer

    let mem =
        getInput 5
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map int

    let part1 () =

        runProgram mem [1] []
        |> finalOutput

    let part2 () =
        0

    let show () =
        showDay
            5
            part1 (Some 9025675)
            part2 None
