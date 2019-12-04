module Day02
    open System
    open Common

    let xs =
        getInput 2
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map int

    let op ip f (mem : int []) =
        let op1 = mem.[mem.[ip + 1]]
        let op2 = mem.[mem.[ip + 2]]

        let result = f op1 op2

        mem.[mem.[ip + 3]] <- result
        ()

    let add ip = op ip (+)

    let multiply ip = op ip (*)

    let initMemory noun verb =
        let mem = Array.copy xs
        mem.[1] <- noun
        mem.[2] <- verb
        mem

    let rec runProgram ip (mem : int []) =
        match mem.[ip] with
        | 1  -> add ip mem;      runProgram (ip + 4) mem
        | 2  -> multiply ip mem; runProgram (ip + 4) mem
        | 99 -> mem.[0]
        | _  -> failwith "unknown opcode"

    let part1 () =

        runProgram 0 (initMemory 12 2)

    let part2 () =

        let rec run noun verb =
            match runProgram 0 (initMemory noun verb) with
            | 19690720 -> (noun, verb)
            | _ ->
                match noun, verb with
                | 100, _ -> failwith "no solution found"
                | n, 100 -> run (n + 1) 0
                | n, v   -> run n (v + 1)

        run 0 0 |> (fun (n, v) -> n * 100 + v)

    let show () =
        showDay
            2
            part1 (Some 4484226)
            part2 (Some 5696)
