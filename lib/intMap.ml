(** Maps with integers as keys. Just an instance of [BatMap]. *)
include BatMap.Make(BatInt)

(** Pretty-printer. *)
let pp ?sep fmtval =
  let open Fmt in using bindings (list ?sep (pair int fmtval))

