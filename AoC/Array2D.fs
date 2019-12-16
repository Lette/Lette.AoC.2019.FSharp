module Array2D

    let private expand array dx dy offsetX offsetY value =
        let xLength = Array2D.length1 array
        let yLength = Array2D.length2 array

        let a' = Array2D.create (xLength + dx) (yLength + dy) value
        Array2D.blit array 0 0 a' offsetX offsetY xLength yLength
        a'

    let expandUp    n zero array = expand array 0 n 0 n zero
    let expandDown  n zero array = expand array 0 n 0 0 zero
    let expandLeft  n zero array = expand array n 0 n 0 zero
    let expandRight n zero array = expand array n 0 0 0 zero

    let expandUpRight   nx ny zero array = expand array nx ny 0  ny zero
    let expandDownRight nx ny zero array = expand array nx ny 0  0  zero
    let expandDownLeft  nx ny zero array = expand array nx ny nx 0  zero
    let expandUpLeft    nx ny zero array = expand array nx ny nx ny zero

    let expandSet zero v x y array =
        let xLength = Array2D.length1 array
        let yLength = Array2D.length2 array

        let array' =
            match x, y with
            | _ when x < 0       && y < 0       -> expandUpLeft    -x                -y                zero array
            | _ when x < xLength && y < 0       -> expandUp                          -y                zero array
            | _ when                y < 0       -> expandUpRight   (x - xLength + 1) -y                zero array
            | _ when x < 0       && y < yLength -> expandLeft      -x                                  zero array
            | _ when x < xLength && y < yLength -> array
            | _ when                y < yLength -> expandRight     (x - xLength + 1)                   zero array
            | _ when x < 0                      -> expandDownLeft  -x                (y - yLength + 1) zero array
            | _ when x < xLength                -> expandDown                        (y - yLength + 1) zero array
            | _                                 -> expandDownRight (x - xLength + 1) (y - yLength + 1) zero array

        array'.[(max 0 x), (max 0 y)] <- v
        array'
