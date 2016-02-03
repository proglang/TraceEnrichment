include Map.Make(BatInt)

let pp ?sep fmtval =
  let open Fmt in using bindings (list ?sep (pair int fmtval))

