module Day04
    open Common

    let xs = (165432, 707912)

    let meetsFirstCriteria n =
        let d6 = n % 10
        let q6 = n / 10
        let d5 = q6 % 10
        let q5 = q6 / 10
        let d4 = q5 % 10
        let q4 = q5 / 10
        let d3 = q4 % 10
        let q3 = q4 / 10
        let d2 = q3 % 10
        let d1 = q3 / 10

        ((d1 = d2) || (d2 = d3) || (d3 = d4) || (d4 = d5) || (d5 = d6))
        &&
        ((d1 <= d2) && (d2 <= d3) && (d3 <= d4) && (d4 <= d5) && (d5 <= d6))

    let firstPass =
        lazy (
            [ (fst xs) .. (snd xs) ]
            |> List.filter meetsFirstCriteria
        )

    let part1 () =

        firstPass.Value
        |> List.length

    let meetsSecondCriteria n =
        let d6 = n % 10
        let q6 = n / 10
        let d5 = q6 % 10
        let q5 = q6 / 10
        let d4 = q5 % 10
        let q4 = q5 / 10
        let d3 = q4 % 10
        let q3 = q4 / 10
        let d2 = q3 % 10
        let d1 = q3 / 10

        (
            ((d1 = d2)                  && not (d2 = d3))
            ||
            ((d2 = d3) && not (d1 = d2) && not (d3 = d4))
            ||
            ((d3 = d4) && not (d2 = d3) && not (d4 = d5))
            ||
            ((d4 = d5) && not (d3 = d4) && not (d5 = d6))
            ||
            ((d5 = d6) && not (d4 = d5)                 )
        )

    let part2 () =
        firstPass.Value
        |> List.filter meetsSecondCriteria
        |> List.length

    let show () =
        showDay
            4
            part1 (Some 1716)
            part2 (Some 1163)
