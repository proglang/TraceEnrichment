include Map.Make(struct type t = int let compare = compare end)

let pp ?sep fmtval =
  let open Fmt in using bindings (list ?sep (pair int fmtval))

