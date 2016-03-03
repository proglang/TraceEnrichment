module type FmtOrderedType = sig
  include BatInterfaces.OrderedType
  val pp: t Fmt.t
end
type 'a diff = Change of 'a * 'a | Remove of 'a | Add of 'a
let pp_diff fmt pp = function
  | Change (vold, vnew) ->
      Format.fprintf pp "@[<hov>%a -> %a@]" fmt vold fmt vnew
  | Remove v ->
      Format.fprintf pp "@[<hov>- %a@]" fmt v
  | Add v ->
      Format.fprintf pp "@[<hov>+ %a@]" fmt v

module type S = sig
  include BatMap.S
  val of_list: (key * 'a) list -> 'a t
  val pp: ?pair_sep: unit Fmt.t -> ?entry_sep: unit Fmt.t ->
    ?entry_frame: ((key * 'a) Fmt.t -> (key * 'a) Fmt.t) ->
    'a Fmt.t -> 'a t Fmt.t
  val delta: ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a diff t
end
module Make(T: FmtOrderedType): S with type key = T.t = struct
  include BatMap.Make(T)
  let of_list l = of_enum (BatList.enum l)
  let pp ?(pair_sep=Fmt.always ",@ ") ?(entry_sep=Fmt.always ";@ ") ?(entry_frame = Fmt.hbox) ppval =
    let open Fmt in
    let frame = entry_frame in
      using bindings (list ~sep:entry_sep (frame (pair ~sep:pair_sep T.pp ppval)))
  let delta equal mold mnew =
    merge (fun _ vold vnew -> match vold, vnew with
             | Some xold, Some xnew ->
                 if equal xold xnew then None else Some (Change(xold, xnew))
             | Some xold, None ->
                 Some (Remove xold)
             | None, Some xnew ->
                 Some (Add xnew)
             | None, None ->
                 None)
      mold mnew
end
