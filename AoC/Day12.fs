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

    let energy m =
        let ev { X = x; Y = y; Z = z } = abs x + abs y + abs z
        (ev m.Position) * (ev m.Velocity)

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

        let x0 = s0.Moons |> List.map (fun m -> (m.Position.X, 0))
        let y0 = s0.Moons |> List.map (fun m -> (m.Position.Y, 0))
        let z0 = s0.Moons |> List.map (fun m -> (m.Position.Z, 0))

        let xCoords ms = ms |> List.map (fun m -> (m.Position.X, m.Velocity.X))
        let yCoords ms = ms |> List.map (fun m -> (m.Position.Y, m.Velocity.Y))
        let zCoords ms = ms |> List.map (fun m -> (m.Position.Z, m.Velocity.Z))

        let xLoop =
            s0
            |> states
            |> Seq.skip 1
            |> Seq.find (fun s -> (xCoords s.Moons) = x0)
            |> fun s -> s.Step
            |> bigint

        let yLoop =
            s0
            |> states
            |> Seq.skip 1
            |> Seq.find (fun s -> (yCoords s.Moons) = y0)
            |> fun s -> s.Step
            |> bigint

        let zLoop =
            s0
            |> states
            |> Seq.skip 1
            |> Seq.find (fun s -> (zCoords s.Moons) = z0)
            |> fun s -> s.Step
            |> bigint

        let gcd' = (gcdI (gcdI xLoop yLoop) zLoop)

        xLoop * yLoop * zLoop / (gcd' ** 3)

    let show () =
        showDay
            12
            part1 (Some 7077)
            part2 (Some 402951477454512I)