include Set.Make(struct type t = int * int let compare = compare end)

let pp ?sep = let open Fmt in using elements (list ?sep (pair int int))

