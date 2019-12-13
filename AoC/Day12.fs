module Day12
    open System
    open Common

    type Vector =
        { X : int; Y : int; Z : int }
        with
            static member Zero = { X = 0; Y = 0; Z = 0 }
            override  __.ToString () = sprintf "<x=%4i, y=%4i, z=%4i>" __.X __.Y __.Z
            static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z }
            static member (-) (a, b) = { X = a.X - b.X; Y = a.Y - b.Y; Z = a.Z - b.Z }

    type Moon =
        { Position : Vector; Velocity : Vector }
        with
            override __.ToString () = sprintf "pos=%O, vel=%O" __.Position __.Velocity

    type Step =
        { Step : int; Moons : Moon list }
        with
            override __.ToString () =
                sprintf
                    "After %i steps:%s%s"
                    __.Step
                    Environment.NewLine
                    (
                        __.Moons
                        |> List.map (sprintf "%O")
                        |> joinStrings Environment.NewLine
                    )

    let positionRegex = "\<x=(\-?[0-9]+), y=(\-?[0-9]+), z=(\-?[0-9]+)\>"

    let toVector =
        function
        | Regex positionRegex [ x; y; z ] -> { X = int x; Y = int y; Z = int z }
        | s -> failwith (sprintf "couldn't parse %s" s)

    let toMoon pos =
        { Position = pos; Velocity = Vector.Zero }

    let input () =
        getInput 12
        |> List.map toVector
        |> List.map toMoon

    let createInitialStep moons =
        { Step = 0; Moons = moons }

    let gravity m1 m2 =
        {
            X = sign (m2.Position.X - m1.Position.X)
            Y = sign (m2.Position.Y - m1.Position.Y)
            Z = sign (m2.Position.Z - m1.Position.Z)
        }

    let applyGravity (moons : Moon list) =
        let rec run (moons : Moon list) acc =

            let buildVelocities (m : Moon) (ms : Moon list) =
                let applyVelocities (m1 : Moon) (m2 : Moon) =
                    let g = gravity m1 m2
                    ({ m2 with Velocity = m2.Velocity - g }, { m1 with Velocity = m1.Velocity + g })
                ms
                |> List.mapFold applyVelocities m
                |> fun (ms, m) -> m :: ms

            match moons with
            | [] -> failwith "unreachable!"
            | m :: [] -> m :: acc |> List.rev
            | m :: ms ->
                match buildVelocities m ms with
                | [] -> failwith "won't happen!"
                | m :: ms -> run ms (m :: acc)

        run moons []
        |> List.map (fun m -> { m with Position = m.Position + m.Velocity })

    let energy moon =
        let vectorEnergy { X = x; Y = y; Z = z } = abs x + abs y + abs z
        (vectorEnergy moon.Position) * (vectorEnergy moon.Velocity)

    let rec states ({ Step = step; Moons = moons} as state) = seq {

        yield state;

        yield!
            states
                {
                    Step = step + 1
                    Moons = applyGravity moons
                }
    }

    let part1 () =

        createInitialStep (input ())
        |> states
        |> Seq.find (fun s -> s.Step = 1000)
        |> fun s -> s.Moons
        |> List.sumBy energy

    let part2 () =

        let s0 = createInitialStep (input ())

        let getElements selector = List.map (fun m -> (selector m.Position, selector m.Velocity))

        let getXs = getElements (fun v -> v.X)
        let getYs = getElements (fun v -> v.Y)
        let getZs = getElements (fun v -> v.Z)

        let x0 = getXs s0.Moons
        let y0 = getYs s0.Moons
        let z0 = getZs s0.Moons

        let check getter zero state value =
            if value > 0 then value else if getter state.Moons = zero then state.Step else 0

        let scannerChecks = (check getXs x0, check getYs y0, check getZs z0)

        let scanner results state =
            scannerChecks
            |> Tuple3.apply state
            |> Tuple3.applyEach results

        s0
        |> states
        |> Seq.skip 1
        |> Seq.scan scanner (0, 0, 0)
        |> Seq.find (Tuple3.forAll ((<) 0))
        |> Tuple3.map bigint
        |> Tuple3.reduce lcm

    let show () =
        showDay
            12
            part1 (Some 7077)
            part2 (Some 402951477454512I)
