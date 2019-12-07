module Computer

    type RunState = Running | Halted | Waiting
    type State = State of mem : int [] * ip : int * input : int list * output : int list * state : RunState

    let private readOp (mem : int []) ip n =
        let modes = mem.[ip] / 100
        let mode = (modes / (pown 10 (n - 1))) % 10

        if mode = 0 then
            mem.[mem.[ip + n]]
        else
            mem.[ip + n]

    let private add (State (mem, ip, i, o, s)) =
        let op1 = readOp mem ip 1
        let op2 = readOp mem ip 2

        let result = (+) op1 op2

        mem.[mem.[ip + 3]] <- result
        State (mem, ip + 4, i, o, s)

    let private multiply (State (mem, ip, i, o, s)) =
        let op1 = readOp mem ip 1
        let op2 = readOp mem ip 2

        let result = (*) op1 op2

        mem.[mem.[ip + 3]] <- result
        State (mem, ip + 4, i, o, s)

    let private read (State (mem, ip, i, o, s)) =
        match i with
        | [] -> State (mem, ip, i, o, Waiting)
        | i :: is ->
            mem.[mem.[ip + 1]] <- i
            State (mem, ip + 2, is, o, s)

    let private write (State (mem, ip, i, o, s)) =
        State (mem, ip + 2, i, (readOp mem ip 1) :: o, s)

    let private jumpIfTrue (State (mem, ip, i, o, s)) =
        match readOp mem ip 1 with
        | 0 -> State (mem, ip + 3, i, o, s)
        | _ -> State (mem, readOp mem ip 2, i, o, s)

    let private jumpIfFalse (State (mem, ip, i, o, s)) =
        match readOp mem ip 1 with
        | 0 -> State (mem, readOp mem ip 2, i, o, s)
        | _ -> State (mem, ip + 3, i, o, s)

    let private lessThan (State (mem, ip, i, o, s)) =
        let op1 = readOp mem ip 1
        let op2 = readOp mem ip 2

        mem.[mem.[ip + 3]] <- if op1 < op2 then 1 else 0
        State (mem, ip + 4, i, o, s)

    let private equals (State (mem, ip, i, o, s)) =
        let op1 = readOp mem ip 1
        let op2 = readOp mem ip 2

        mem.[mem.[ip + 3]] <- if op1 = op2 then 1 else 0
        State (mem, ip + 4, i, o, s)

    let private halt (State (mem, ip, i, o, _)) =
        State (mem, ip, i, o, Halted)

    let createInitialState mem i =
        State (mem, 0, i, [], Running)

    let provideInput i (State (mem, ip, is, o, s)) =
        State (mem, ip, (i :: is), o, if s = Waiting then Running else s)

    let isWaiting (State (_, _, _, _, s)) = s = Waiting

    let isHalted (State (_, _, _, _, s)) = s = Halted

    let rec runProgram ((State (mem, ip, i, o, rs)) as state) =

        if rs <> Running then
            state
        else

            let go f = state |> f |> runProgram

            match mem.[ip] % 100 with
            | 1  -> go add
            | 2  -> go multiply
            | 3  -> go read
            | 4  -> go write
            | 5  -> go jumpIfTrue
            | 6  -> go jumpIfFalse
            | 7  -> go lessThan
            | 8  -> go equals
            | 99 -> go halt
            | i  -> failwith (sprintf "unknown opcode: %i" i)

    let finalOutput (State (_, _, _, o, rs)) =
        match rs, List.tryHead o with
        | Halted, Some x -> x
        | Halted, None   -> failwith "no output!"
        | _              -> failwith "not halted!"

    let lastOutput (State (_, _, _, o, _)) =
        match List.tryHead o with
        | Some x -> x
        | None   -> failwith "no output!"

    let finalValueAt address (State (mem, _, _, _, rs)) =
        match rs with
        | Halted -> mem.[address]
        | _      -> failwith "not halted!"
