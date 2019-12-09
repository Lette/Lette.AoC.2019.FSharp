module Day02
    open Common
    open Computer

    let xs =
        getInput 2
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map parseBigint

    let initMemory noun verb =
        let mem = Array.copy xs
        mem.[1] <- bigint noun
        mem.[2] <- bigint verb
        mem

    let finalValueAtZero noun verb =

        createInitialState (initMemory noun verb) []
        |> runProgram
        |> finalValueAt 0

    let part1 () =

        finalValueAtZero 12 2

    let part2 () =

        let rec run noun verb =
            match finalValueAtZero noun verb with
            | BigInt 19690720I -> (noun, verb)
            | _ ->
                match noun, verb with
                | 100, _ -> failwith "no solution found"
                | n, 100 -> run (n + 1) 0
                | n, v   -> run n (v + 1)

        run 0 0 |> (fun (n, v) -> n * 100 + v)

    let show () =
        showDay
            2
            part1 (Some 4484226I)
            part2 (Some 5696)
