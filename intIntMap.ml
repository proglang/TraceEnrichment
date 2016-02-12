(** Maps with integer pairs as keys. Essentially, just an instance of [BatMap]. *)
include BatMap.Make(BatTuple.Tuple2.Comp(BatInt)(BatInt))
(** Pretty-printer. *)
let pp ?sep fmtval =
  let open Fmt in using bindings (list ?sep (pair (pair int int) fmtval))

