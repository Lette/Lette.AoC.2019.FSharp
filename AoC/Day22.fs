module Day22
    open System
    open Common

    type Shuffle =
        | Deal of int
        | Cut of int
        | Reverse

    let toShuffle =
        function
        | Regex "deal with increment (\d+)" [ n ] -> Deal (int n)
        | Regex "cut (\-?\d+)" [ n ] -> Cut (int n)
        | Regex "deal into new stack" [] -> Reverse
        | s -> failwith (sprintf "can't understand %s" s)

    let doShuffle deckSize card shuffle =
        match shuffle with
        | Reverse -> deckSize - 1 - card
        | Cut c   -> (card - c + deckSize) % deckSize
        | Deal n  -> (card * n) % deckSize

    let xs () =
        getInput 22
        //[
            //"deal into new stack"
            //"cut 3"
            //"cut -4"
            //"deal with increment 3"
        //]
        |> List.map toShuffle

    let part1 () =
        xs ()
        |> List.fold (doShuffle 10007) 2019

    let part2 () =
        // 119315717514047 cards
        // 101741582076661 sets of shuffles

        // both are primes!

        // what number is on the card that ends up in position 2020?

        // check if cards in position 2020 repeats?

        0

    let show () =
        showDay
            22
            part1 (Some 6831)
            part2 None
