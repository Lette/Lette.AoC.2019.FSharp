module Computer
    open Common

    type RunState = Running | Halted | Waiting

    type State = {
        Memory : bigint []
        Ip : int
        Input : bigint list
        Output : bigint list
        State : RunState
        Rbo : int
    }

    let private read           s address       =
        if address < Array.length s.Memory then
            s.Memory.[address]
        else
            failwith (sprintf "can't read address: %i - mem size: %i" address (Array.length s.Memory))

    let private readRef        s address       = read s (read s address |> int)
    let private readRelIp      s offset        = read s (s.Ip + offset)
    let private readRelRbo     s offset        = read s (s.Rbo + offset)
    let private readRefRelIp   s offset        = read s (readRelIp s offset |> int)
    let private readRefRelRbo  s offset        = read s (readRelRbo s offset |> int)

    let private write          s address value =
        if address < Array.length s.Memory then
            s.Memory.[address] <- value
        else
            failwith (sprintf "can't write address: %i - mem size: %i" address (Array.length s.Memory))

    let private writeRef       s address value = write s (read s address |> int) value
    let private writeRefRelIp  s offset  value = write s (readRelIp s offset |> int) value
    let private writeRefRelRbo s offset  value = write s (s.Rbo + offset) value

    let incrementIp n  s = { s with Ip = s.Ip + n }
    let setIp       n  s = { s with Ip = n }
    let addOutput   o  s = { s with Output = o :: s.Output }
    let setInputs   is s = { s with Input = is }
    let addInput    i  s = { s with Input = i :: s.Input }
    let setWaiting     s = { s with State = Waiting }
    let setHalted      s = { s with State = Halted }
    let setRbo      n  s = { s with Rbo = n }
    let setMemory a v  s = write s a v; s
    let clearOutputs   s = { s with Output = [] }
    let popOutput      s = (s.Output, clearOutputs s)

    let getParameterMode s n =
        let modes = readRelIp s 0 |> int |> flip (/) 100
        (modes / (pown 10 (n - 1))) % 10

    let private readOp s n =
        match getParameterMode s n with
        | 0 -> readRefRelIp s n
        | 1 -> readRelIp s n
        | 2 -> readRelRbo s (readRelIp s n |> int)
        | i -> failwith (sprintf "read: unknown parameter mode: %i" i)

    let private writeResult s n result =
        match getParameterMode s n with
        | 0 -> writeRefRelIp s n result
        | 1 -> failwith "writes in immediate mode?"
        | 2 -> writeRefRelRbo s (readRelIp s n |> int) result
        | i -> failwith (sprintf "write: unknown parameter mode: %i" i)

    let private add s =
        (readOp s 1, readOp s 2)
        ||> (+)
        |> writeResult s 3

        incrementIp 4 s

    let private multiply s =
        (readOp s 1, readOp s 2)
        ||> (*)
        |> writeResult s 3

        incrementIp 4 s

    let private readInput s =
        match s.Input with
        | [] -> setWaiting s
        | i :: is ->
            writeResult s 1 i
            s |> incrementIp 2 |> setInputs is

    let private writeOutput s =
        s |> incrementIp 2 |> addOutput (readOp s 1)

    let private jumpIfTrue s =
        match readOp s 1 with
        | BigInt 0I -> incrementIp 3 s
        | _         -> setIp (readOp s 2 |> int) s

    let private jumpIfFalse s =
        match readOp s 1 with
        | BigInt 0I -> setIp (readOp s 2 |> int) s
        | _         -> incrementIp 3 s

    let private lessThan s =
        let op1 = readOp s 1
        let op2 = readOp s 2

        writeResult s 3 (if op1 < op2 then 1I else 0I)

        incrementIp 4 s

    let private equals s =
        let op1 = readOp s 1
        let op2 = readOp s 2

        writeResult s 3 (if op1 = op2 then 1I else 0I)

        incrementIp 4 s

    let private adjustRbo s =
        s
        |> setRbo (s.Rbo + (readOp s 1 |> int))
        |> incrementIp 2

    let private halt s =
        setHalted s

    let createInitialState mem i =
        { Memory = mem; Ip = 0; Input = i; Output = []; State = Running; Rbo = 0 }

    let expandMemory size s =
        let expand arr =
            let arr' = Array.create size 0I
            Array.blit arr 0 arr' 0 (Array.length arr)
            arr'
        { s with Memory = expand s.Memory }

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

            match (readRelIp state 0 |> int) |> flip (%) 100 with
            | 1  -> go add
            | 2  -> go multiply
            | 3  -> go readInput
            | 4  -> go writeOutput
            | 5  -> go jumpIfTrue
            | 6  -> go jumpIfFalse
            | 7  -> go lessThan
            | 8  -> go equals
            | 9  -> go adjustRbo
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
