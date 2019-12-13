module Tuple3

    let map f (a, b, c) =
        (f a, f b, f c)

    let mapEach (f1, f2, f3) (a, b, c) =
        (f1 a, f2 b, f3 c)

    let reduce f (a, b, c) =
        f (f a b) c

    let apply v (f1, f2, f3) =
        (f1 v, f2 v, f3 v)

    let applyEach (a, b, c) (f1, f2, f3) =
        (f1 a, f2 b, f3 c)

    let zip (a1, b1, c1) (a2, b2, c2) =
        ((a1, a2), (b1, b2), (c1, c2))

    let defaultValue d (a, b, c) =
        (
            Option.defaultValue d a,
            Option.defaultValue d b,
            Option.defaultValue d c
        )

    let defaultValueEach (da, db, dc) (a, b, c) =
        (
            Option.defaultValue da a,
            Option.defaultValue db b,
            Option.defaultValue dc c
        )

    let forAll predicate (a, b, c) =
        predicate a && predicate b && predicate c
