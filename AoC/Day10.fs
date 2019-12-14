module Day10
    open Common

    let parseChar y x =
        function
        | '#' -> Some (x, y)
        | '.' -> None
        | c   -> failwith (sprintf "unrecognised input: %c" c)

    let parseRow y s =
        Seq.toList s
        |> List.mapi (parseChar y)

    let asteroids =
        lazy (
            getInput 10
            |> List.mapi parseRow
            |> List.concat
            |> List.choose id
        )

    let getDirectionAndDistance (x, y) (u, v) =
        match u - x, v - y with
        | 0, 0 -> (0, 0, 0)
        | n, 0 -> (sign n, 0, abs n)
        | 0, m -> (0, sign m, abs m)
        | n, m ->
            let d = gcd (abs n) (abs m)
            (n / d, m / d, d)

    let getDirection a b =
        getDirectionAndDistance a b
        |> fun (x, y, _) -> (x, y)

    let countVisiblesFrom a =
        asteroids.Value
        |> List.map (fun b -> getDirection a b)
        |> List.distinct
        |> List.length
        |> flip (-) 1

    let home () =
        asteroids.Value 
        |> List.map (fun x -> (x, countVisiblesFrom x))
        |> List.sortByDescending snd
        |> List.head

    let part1 () =

        home () |> snd

    let part2 () =
        
        let runGiantLaser asteroids =

            let rec outer (asteroids, acc) =

                let rec inner asteroids newAsteroids acc =
                    match asteroids with
                    | [] -> List.rev newAsteroids, acc
                    | x :: xs ->
                        match x with
                        | []      -> failwith "can't happen!"
                        | y :: [] -> inner xs        newAsteroids  (y :: acc)
                        | y :: ys -> inner xs (ys :: newAsteroids) (y :: acc)

                match asteroids with
                | [] -> List.rev acc
                | _  -> outer (inner asteroids [] acc)

            outer (asteroids, [])

        let direction (_, (dx, dy, _)) = (dx, dy)

        let toAsteroidsOrderedByDistance (k, vs) =
            k,
            vs
            |> List.sortBy (fun (_, (_, _, d)) -> d)
            |> List.map fst

        let clockwise ((dx, dy), _) =
            (piOver2 - atan2 (float -dy) (float dx) + twoPi) % twoPi

        let home = home () |> fst

        let directionsFrom here there =
            (there, getDirectionAndDistance here there)

        asteroids.Value                            //             (x, y) list
        |> List.except [home]                      //             (x, y) list
        |> List.map (directionsFrom home)          //            ((x, y), (dx, dy, d)) list
        |> List.groupBy direction                  // ((dx, dy), ((x, y), (dx, dy, d)) list) list
        |> List.map toAsteroidsOrderedByDistance   // ((dx, dy),  (x, y)               list) list
        |> List.sortBy clockwise                   // ((dx, dy),  (x, y)               list) list
        |> List.map snd                            //             (x, y)               list  list
        |> runGiantLaser                           //             (x, y)               list
        |> List.item 199                           //             (x, y)
        |> Tuple2.mapFst ((*) 100)
        |> Tuple2.reduce (+)

    let show () =
        showDay
            10
            part1 (Some 253)
            part2 (Some 815)
