module Computer
    open Common

    type RunState = Running | Halted | Waiting

    type State = {
        Memory : int []
        Ip : int
        Input : int list
        Output : int list
        State : RunState
    }

    let private read        s address       = s.Memory.[address]
    let private readRef     s address       = read s (read s address)
    let private readRel     s offset        = read s (s.Ip + offset)
    let private readRefRel  s offset        = read s (readRel s offset)
    let private write       s address value = s.Memory.[address] <- value
    let private writeRef    s address value = write s (read s address) value
    let private writeRefRel s offset  value = write s (readRel s offset) value

    let incrementIp n  s = { s with Ip = s.Ip + n }
    let setIp       n  s = { s with Ip = n }
    let addOutput   o  s = { s with Output = o :: s.Output }
    let setInputs   is s = { s with Input = is }
    let addInput    i  s = { s with Input = i :: s.Input }
    let setWaiting     s = { s with State = Waiting }
    let setHalted      s = { s with State = Halted }

    let private readOp s n =
        let modes = readRel s 0 / 100
        let mode = (modes / (pown 10 (n - 1))) % 10

        if mode = 0 then
            readRefRel s n
        else
            readRel s n

    let private add s =
        (readOp s 1, readOp s 2)
        ||> (+)
        |> writeRefRel s 3

        incrementIp 4 s

    let private multiply s =
        (readOp s 1, readOp s 2)
        ||> (*)
        |> writeRefRel s 3

        incrementIp 4 s

    let private readInput s =
        match s.Input with
        | [] -> setWaiting s
        | i :: is ->
            writeRefRel s 1 i
            s |> incrementIp 2 |> setInputs is

    let private writeOutput s =
        s |> incrementIp 2 |> addOutput (readOp s 1)

    let private jumpIfTrue s =
        match readOp s 1 with
        | 0 -> incrementIp 3 s
        | _ -> setIp (readOp s 2) s

    let private jumpIfFalse s =
        match readOp s 1 with
        | 0 -> setIp (readOp s 2) s
        | _ -> incrementIp 3 s

    let private lessThan s =
        let op1 = readOp s 1
        let op2 = readOp s 2

        writeRefRel s 3 (if op1 < op2 then 1 else 0)

        incrementIp 4 s

    let private equals s =
        let op1 = readOp s 1
        let op2 = readOp s 2

        writeRefRel s 3 (if op1 = op2 then 1 else 0)

        incrementIp 4 s

    let private halt s =
        setHalted s

    let createInitialState mem i =
        { Memory = mem; Ip = 0; Input = i; Output = []; State = Running }

    let provideInput i s =
        s
        |> addInput i
        |> fun s -> { s with State = (if s.State = Waiting then Running else s.State) }

    let isWaiting s = s.State = Waiting

    let isHalted s = s.State = Halted

    let rec runProgram state =

        if state.State <> Running then
            state
        else

            let go f = state |> f |> runProgram

            match readRel state 0 % 100 with
            | 1  -> go add
            | 2  -> go multiply
            | 3  -> go readInput
            | 4  -> go writeOutput
            | 5  -> go jumpIfTrue
            | 6  -> go jumpIfFalse
            | 7  -> go lessThan
            | 8  -> go equals
            | 99 -> go halt
            | i  -> failwith (sprintf "unknown opcode: %i" i)

    let finalOutput state =
        match state.State, List.tryHead state.Output with
        | Halted, Some x -> x
        | Halted, None   -> failwith "no output!"
        | _              -> failwith "not halted!"

    let lastOutput state =
        match List.tryHead state.Output with
        | Some x -> x
        | None   -> failwith "no output!"

    let finalValueAt address state =
        match state.State with
        | Halted -> read state address
        | _      -> failwith "not halted!"
