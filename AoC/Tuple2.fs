module Tuple2

    let mapFst f (a, b) = (f a, b)

    let mapSnd f (a, b) = (a, f b)
