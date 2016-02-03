include Set.Make(BatTuple.Tuple2.Comp(BatInt)(BatInt))
let pp ?sep = let open Fmt in using elements (list ?sep (pair int int))

