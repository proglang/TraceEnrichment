include Map.Make(BatTuple.Tuple2.Comp(BatInt)(BatInt))
let pp ?sep fmtval =
  let open Fmt in using bindings (list ?sep (pair (pair int int) fmtval))

