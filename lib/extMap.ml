module type FmtOrderedType = sig
  include BatInterfaces.OrderedType
  val pp: t Fmt.t
end
module Make(S: FmtOrderedType) = struct
  include BatMap.Make(S)
  let of_list l = of_enum (BatList.enum l)
  let pp ?pair_sep ?entry_sep ?entry_frame ppval =
    let frame = match entry_frame with
      | Some frame -> frame
      | None -> (fun fmt pp value -> fmt pp value)
    in let open Fmt in
      using bindings (list ?sep:entry_sep (frame (pair ?sep:pair_sep S.pp ppval)))
end
