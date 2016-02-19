(** Sets of integer pairs. Mostly just an instance of [BatSet.S]. *)
include BatSet.Make(BatTuple.Tuple2.Comp(BatInt)(BatInt))
let pp ?sep = let open Fmt in using elements (list ?sep (pair int int))

