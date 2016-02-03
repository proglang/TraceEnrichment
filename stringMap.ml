include BatMap.Make(String)
let pp ?sep fmtval = 
  let open Fmt in using bindings (list ?sep (pair string fmtval))
let of_list l = of_enum (BatList.enum l)
