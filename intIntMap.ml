include Map.Make(struct type t = int * int let compare = compare end)
let pp ?sep fmtval =
  let open Fmt in using bindings (list ?sep (pair (pair int int) fmtval))

