module Day16
    open Common

    let xs () =
        getInput 16
        |> List.head
        |> Seq.toList
        |> List.map (string >> int)

    let basePattern = [| 0; 1; 0; -1 |]

    let coefficientAt col row =
        if col < row then
            0
        else
            let x' = ((col + 1) / (row + 1)) % 4
            basePattern.[x']

    let resultAt digits index =
        digits
        |> List.mapi (fun col digit -> coefficientAt col index * digit)
        |> List.sum
        |> abs
        |> flip (%) 10

    let fft digits =
        [0 .. List.length digits - 1 ]
        |> List.map (resultAt digits)

    let fft' digits =
        Seq.scanBack (fun d s -> (d + s) % 10) digits 0

    let digitsToInt count ds =
        ds
        |> Seq.take count
        |> Seq.fold (fun s d -> s * 10 + d) 0

    let toOutput (xs : int seq) =
        xs
        |> digitsToInt 8

    let part1 () =

        xs ()
        |> iterate 100 fft
        |> toOutput

    let part2 () =

        let input = xs ()
        let length = List.length input * 10000
        let offset = input |> Seq.take 7 |> Seq.fold (fun s d -> s * 10 + d) 0

        input
        |> List.toSeq
        |> cycle
        |> Seq.skip offset
        |> Seq.take (length - offset)
        |> iterate 100 fft'
        |> toOutput

    let show () =
        showDay
            16
            part1 (Some 10332447)
            part2 (Some 14288025)
