module Day01
    open Common

    let xs =
        getInput 1
        |> List.map int

    let fuelReq mass = (mass / 3) - 2

    let fuelReqRec mass =

        let rec trFuelReqs lastMass acc =
            let newMass = fuelReq lastMass
            if newMass > 0 then
                trFuelReqs newMass (acc + newMass)
            else
                acc

        trFuelReqs mass 0

    let part1 () =

        xs |> List.sumBy fuelReq

    let part2 () =

        xs |> List.sumBy fuelReqRec

    let show () =
        showDay
            1
            part1 (Some 3376997)
            part2 (Some 5062623)
