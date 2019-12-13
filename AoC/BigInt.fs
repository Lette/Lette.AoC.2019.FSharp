[<AutoOpen>]
module BigInt
    open System.Numerics

    let bigint (n : int) = bigint(n)

    let parseBigint (s : string) = BigInteger.Parse(s)

    let (|BigInt|_|) (n : BigInteger) (input : BigInteger) = if n = input then Some () else None

    let rec gcd a b =
        if b = 0I then
            a
        else
            gcd b (a % b)

    let lcm a b = a * b / (gcd a b)
