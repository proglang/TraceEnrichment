type scope =
  | Global
  | Local of int
  [@@deriving ord, eq]
let pp_scope pp = function
  | Global -> Fmt.string pp "Global"
  | Local env -> Format.fprintf pp "Local(%d)" env

module Reference = struct
  type t =
    | Field of TypesJS.fieldref
    | Variable of scope * string
    [@@deriving ord, eq]
  let pp pp = function
    | Field (obj, field) ->
        Format.fprintf pp "%a:%s" TypesJS.pp_objectid obj field
    | Variable (scope, name) ->
        Format.fprintf pp "%a:%s" pp_scope scope name
end
open Reference
type reference = Reference.t =
  | Field of TypesJS.fieldref
  | Variable of scope * string

let pp_reference = Reference.pp
let equal_reference = Reference.equal

let get_fieldref = function
  | Field(obj, fld) -> Some (obj, fld)
  | _ -> None
let is_global = function
  | Variable (Global, _) -> true
  | _ -> false
let get_name = function
  | Variable (_, name) -> Some name
  | _ -> None


module ReferenceMap = ExtMap.Make(Reference)
let pp_reference_map = ReferenceMap.pp

let global_object = TypesJS.Object 0

let reference_of_field base offset =
  try
    Field (TypesJS.objectid_of_jsval base, offset)
  with TypesJS.NotAnObject ->
    failwith ("Trying to build field access to " ^ Fmt.to_to_string TypesJS.pp_jsval base)

let reference_of_fieldref (base, offset) = Field (base, offset)
let reference_of_local_name scope name = Variable (Local scope, name)
let reference_of_name globals_are_properties bindings name =
  if StringMap.mem name bindings then
    StringMap.find name bindings
  else if globals_are_properties then
    Field(global_object, name)
  else
    Variable(Global, name)

module VersionedReference = struct
  type t = reference * int [@@deriving show]
  let equal: t -> t -> bool = (=)
  let compare: t -> t -> int = Pervasives.compare
  let hash = let open TypesJS in function
    | (Field (Object id, name), ver)
    | (Field (Function (id, _), name), ver)
    | (Field (Other (_, id), name), ver) ->
        0 + 2 * (id + 5 * (ver + 5 * Hashtbl.hash name))
    | (Variable (Local id, name), ver) ->
        1 + 2 * (id + 5 * (ver + 5 * Hashtbl.hash name))
    | (Variable (Global, name), ver) ->
        1 + 2 * (5 * (ver + 5 * Hashtbl.hash name))
end
type versioned_reference = VersionedReference.t
let pp_versioned_reference = VersionedReference.pp

module VersionedReferenceMap = CCPersistentHashtbl.Make(VersionedReference)
module VersionedReferenceSet = Set.Make(VersionedReference);;
let pp_versioned_reference_map fmtval =
  VersionedReferenceMap.print pp_versioned_reference fmtval
let pp_versioned_reference_set =
  let open Fmt in
    using VersionedReferenceSet.elements (list pp_versioned_reference)

type points_to_map = TypesJS.jsval VersionedReferenceMap.t

let pp_points_to_map = pp_versioned_reference_map TypesJS.pp_jsval


