module Computer

    type State = State of mem : int [] * ip : int * input : int list * output : int list

    let private readOp (mem : int []) ip n =
        let modes = mem.[ip] / 100
        let mode = (modes / (pown 10 (n - 1))) % 10

        if mode = 0 then
            mem.[mem.[ip + n]]
        else
            mem.[ip + n]

    let private add (State (mem, ip, i, o)) =
        let op1 = readOp mem ip 1
        let op2 = readOp mem ip 2

        let result = (+) op1 op2

        mem.[mem.[ip + 3]] <- result
        State (mem, ip + 4, i, o)

    let private multiply (State (mem, ip, i, o)) =
        let op1 = readOp mem ip 1
        let op2 = readOp mem ip 2

        let result = (*) op1 op2

        mem.[mem.[ip + 3]] <- result
        State (mem, ip + 4, i, o)

    let private read (State (mem, ip, i, o)) =
        match i with
        | [] -> failwith "cannot read - no input!"
        | i :: is ->
            mem.[mem.[ip + 1]] <- i
            State (mem, ip + 2, is, o)

    let private write (State (mem, ip, i, o)) =
        State (mem, ip + 2, i, (readOp mem ip 1) :: o)

    let private jumpIfTrue (State (mem, ip, i, o)) =
        match readOp mem ip 1 with
        | 0 -> State (mem, ip + 3, i, o)
        | _ -> State (mem, readOp mem ip 2, i, o)

    let private jumpIfFalse (State (mem, ip, i, o)) =
        match readOp mem ip 1 with
        | 0 -> State (mem, readOp mem ip 2, i, o)
        | _ -> State (mem, ip + 3, i, o)

    let private lessThan (State (mem, ip, i, o)) =
        let op1 = readOp mem ip 1
        let op2 = readOp mem ip 2

        mem.[mem.[ip + 3]] <- if op1 < op2 then 1 else 0
        State (mem, ip + 4, i, o)

    let private equals (State (mem, ip, i, o)) =
        let op1 = readOp mem ip 1
        let op2 = readOp mem ip 2

        mem.[mem.[ip + 3]] <- if op1 = op2 then 1 else 0
        State (mem, ip + 4, i, o)

    let runProgram (mem : int []) i o =

        let rec run state =
            let (State (_, ip, _, _)) = state

            let go f = state |> f |> run

            match mem.[ip] % 100 with
            | 1  -> go add
            | 2  -> go multiply
            | 3  -> go read
            | 4  -> go write
            | 5  -> go jumpIfTrue
            | 6  -> go jumpIfFalse
            | 7  -> go lessThan
            | 8  -> go equals
            | 99 -> state
            | i  -> failwith (sprintf "unknown opcode: %i" i)

        run (State (mem, 0, i, o))

    let finalOutput (State (_, _, _, o)) =
        match List.tryHead o with
        | None   -> failwith "no output!"
        | Some x -> x

    let finalValueAt address (State (mem, _, _, _)) =
        mem.[address]
