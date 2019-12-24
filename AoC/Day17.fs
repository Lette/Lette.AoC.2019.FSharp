module Day17
    open System
    open Common
    open Computer

    let mem () =
        getInput 17
        |> List.head
        |> (fun s -> s.Split ',')
        |> Array.map parseBigint

    let part1 () =

        let createMap ds =
            ds
            |> List.rev
            |> List.map (int >> char)
            |> List.toArray
            |> String
            |> fun s -> s.Split '\n'
            |> Array.map Seq.toArray
            |> Array.filter (fun arr -> Array.length arr > 0)

        let findIntersections array =
            let length1 = Array2D.length1 array
            let length2 = Array2D.length2 array

            let getChar x y = Array2D.get array y x

            let isIntersection x y =
                (getChar x y) = '#'
                &&
                (getChar (x - 1) y) = '#'
                &&
                (getChar (x + 1) y) = '#'
                &&
                (getChar x (y - 1)) = '#'
                &&
                (getChar x (y + 1)) = '#'

            seq {
                for y in [1 .. length1 - 1] do
                    for x in [ 1 .. length2 - 1] do
                        if isIntersection x y then
                            yield (x, y)
            }

        let printMap (map : char [] []) =
            printfn ""
            map
            |> Array.map String
            |> Array.iter (printfn ":%s")
            map

        createInitialState (mem ()) []
        |> expandMemory 10000
        |> runProgram
        |> popOutput
        |> fst
        |> createMap
        //|> printMap
        |> array2D
        |> findIntersections
        |> Seq.map (fun (x, y) -> x * y)
        |> Seq.sum

    let part2 () =

        let inputs =
            [
                "A,B,A,B,C,C,B,A,B,C\n"
                "L,10,R,10,L,10,L,10\n"
                "R,10,R,12,L,12\n"
                "R,12,L,12,R,6\n"
                "n\n"
            ]
            |> List.map Seq.toList
            |> List.concat
            |> List.map (int >> bigint)

        createInitialState (mem ()) inputs
        |> expandMemory 10000
        |> setMemory 0 2I
        |> runProgram
        |> popOutput
        |> fst
        |> List.head

    let show () =
        showDay
            17
            part1 (Some 7720)
            part2 (Some 1681189I)
