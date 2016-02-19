(** Maps with strings as keys. This is essentially just an instance of [BatMap]. *)
include BatMap.Make(String)
(** Pretty-printer. *)
let pp ?sep fmtval = 
  let open Fmt in using bindings (list ?sep (pair string fmtval))
(** Generate map from a list of (key, value) pairs. *)
let of_list l = of_enum (BatList.enum l)
