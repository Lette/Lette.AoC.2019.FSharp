module Day04
    open Common

    let xs = (165432, 707912)

    let isValid n =
        let d6 = n % 10
        let q6 = n / 10
        let d5 = q6 % 10
        let q5 = q6 / 10
        let d4 = q5 % 10
        let q4 = q5 / 10
        let d3 = q4 % 10
        let q3 = q4 / 10
        let d2 = q3 % 10
        let q2 = q3 / 10
        let d1 = q2

        ((d1 = d2) || (d2 = d3) || (d3 = d4) || (d4 = d5) || (d5 = d6))
        &&
        ((d1 <= d2) && (d2 <= d3) && (d3 <= d4) && (d4 <= d5) && (d5 <= d6))

    let part1 () =

        let last = (snd xs) + 1

        let rec run n acc =
            match n, isValid n with
            | (x, _) when x = last -> acc
            | _, v                 -> run (n + 1) (if v then acc + 1 else acc)

        run (fst xs) 0

    let part2 () =
        0

    let show () =
        showDay
            4
            part1 (Some 1716)
            part2 None
