[<AutoOpen>]
module Tuple3

    let defaultValue (da, db, dc) (a, b, c) =
        (Option.defaultValue da a, Option.defaultValue db b, Option.defaultValue dc c)
