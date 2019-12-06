module Computer

    type State = State of ip : int * input : int list * output : int list

    let private readOp (mem : int []) ip n offset =
        let modes = mem.[ip] / 10
        let mode = (modes / (pown 10 n)) % 10

        if mode = 0 then
            mem.[mem.[ip + offset]]
        else
            mem.[ip + offset]

    let private add (mem : int []) (State (ip, i, o)) =
        let op1 = readOp mem ip 1 1
        let op2 = readOp mem ip 2 2

        let result = (+) op1 op2

        mem.[mem.[ip + 3]] <- result
        State (ip + 4, i, o)

    let private multiply (mem : int []) (State (ip, i, o)) =
        let op1 = readOp mem ip 1 1
        let op2 = readOp mem ip 2 2

        let result = (*) op1 op2

        mem.[mem.[ip + 3]] <- result
        State (ip + 4, i, o)

    let private read (mem : int []) (State (ip, i, o)) =
        match i with
        | [] -> failwith "cannot read - no input!"
        | i :: is ->
            mem.[mem.[ip + 1]] <- i
            State (ip + 2, is, o)

    let private write (mem : int []) (State (ip, i, o)) =
        State (ip + 2, i, (readOp mem ip 1 1) :: o)

    let private jumpIfTrue (mem : int []) (State (ip, i, o)) =
        match readOp mem ip 1 1 with
        | 0 -> State (ip + 3, i, o)
        | _ -> State (readOp mem ip 2 2, i, o)

    let private jumpIfFalse (mem : int []) (State (ip, i, o)) =
        match readOp mem ip 1 1 with
        | 0 -> State (readOp mem ip 2 2, i, o)
        | _ -> State (ip + 3, i, o)

    let private lessThan (mem : int []) (State (ip, i, o)) =
        let op1 = readOp mem ip 1 1
        let op2 = readOp mem ip 2 2

        mem.[mem.[ip + 3]] <- if op1 < op2 then 1 else 0
        State (ip + 4, i, o)

    let private equals (mem : int []) (State (ip, i, o)) =
        let op1 = readOp mem ip 1 1
        let op2 = readOp mem ip 2 2

        mem.[mem.[ip + 3]] <- if op1 = op2 then 1 else 0
        State (ip + 4, i, o)

    let runProgram (mem : int []) i o =

        let rec run state =
            let (State (ip, _, _)) = state

            match mem.[ip] % 100 with
            | 1  -> state |> add         mem |> run
            | 2  -> state |> multiply    mem |> run
            | 3  -> state |> read        mem |> run
            | 4  -> state |> write       mem |> run
            | 5  -> state |> jumpIfTrue  mem |> run
            | 6  -> state |> jumpIfFalse mem |> run
            | 7  -> state |> lessThan    mem |> run
            | 8  -> state |> equals      mem |> run
            | 99 -> state
            | i  -> failwith (sprintf "unknown opcode: %i" i)

        run (State (0, i, o))

    let finalOutput (State (_, _, o)) =
        match List.tryHead o with
        | None   -> failwith "no output!"
        | Some x -> x
