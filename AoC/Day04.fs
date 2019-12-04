module Day04
    open Common

    let xs = (165432, 707912)

    let toDigits n =
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

        [ d1; d2; d3; d4; d5; d6 ]

    let isSameDigit =
        function
        | a :: b :: [] when a = b -> true
        | _                       -> false

    let isNonDecreasing =
        function
        | a :: b :: [] when a <= b -> true
        | _                        -> false
        
    let twoAdjacentDigitsAreTheSame =
        List.windowed 2
        >> List.exists isSameDigit

    let digitsNeverDecrease =
        List.windowed 2
        >> List.forall isNonDecreasing

    let firstPass =
        lazy (
            [ (fst xs) .. (snd xs) ]
            |> List.map toDigits
            |> List.filter twoAdjacentDigitsAreTheSame
            |> List.filter digitsNeverDecrease
        )

    let isExactlyAPair =
        function
        | a :: b :: c :: d :: [] when a < b && b = c && c < d -> true
        | _ -> false

    let pairsAreNotPartOfLargerGroup =
        cons 0
        >> flip (@) [10]
        >> List.windowed 4
        >> List.exists isExactlyAPair

    let part1 () =

        firstPass.Value
        |> List.length

    let part2 () =

        firstPass.Value
        |> List.filter pairsAreNotPartOfLargerGroup
        |> List.length

    let show () =
        showDay
            4
            part1 (Some 1716)
            part2 (Some 1163)
