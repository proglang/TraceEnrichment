module type FmtOrderedType = sig
  include BatInterfaces.OrderedType
  val pp: t Fmt.t
end
module type S = sig
  include BatMap.S
  val of_list: (key * 'a) list -> 'a t
  val pp: ?pair_sep: unit Fmt.t -> ?entry_sep: unit Fmt.t ->
    ?entry_frame: ((key * 'a) Fmt.t -> (key * 'a) Fmt.t) ->
    'a Fmt.t -> 'a t Fmt.t
end
module Make(T: FmtOrderedType): S with type key = T.t = struct
  include BatMap.Make(T)
  let of_list l = of_enum (BatList.enum l)
  let pp ?(pair_sep=Fmt.always ",@ ") ?(entry_sep=Fmt.always ";@ ") ?(entry_frame = Fmt.hbox) ppval =
    let open Fmt in
    let frame = entry_frame in
      using bindings (list ~sep:entry_sep (frame (pair ~sep:pair_sep T.pp ppval)))
end
