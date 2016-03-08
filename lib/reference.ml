type scope =
  | Global
  | Local of int
  [@@deriving ord, eq]
let pp_scope pp = function
  | Global -> Fmt.string pp "Global"
  | Local env -> Format.fprintf pp "Local(%d)" env

module Reference = struct
  type t =
    | Field of Types.fieldref
    | Variable of scope * string
    [@@deriving ord, eq]
  let pp pp = function
    | Field (obj, field) ->
        Format.fprintf pp "%a:%s" Types.pp_objectid obj field
    | Variable (scope, name) ->
        Format.fprintf pp "%a:%s" pp_scope scope name
end
open Reference
type reference = Reference.t =
  | Field of Types.fieldref
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

let global_object = Types.Object 0

let reference_of_field base offset =
  try
    Field (Types.objectid_of_jsval base, offset)
  with Types.NotAnObject ->
    failwith ("Trying to build field access to " ^ Fmt.to_to_string Types.pp_jsval base)

let reference_of_fieldref (base, offset) = Field (base, offset)
let reference_of_local_name scope name = Variable (Local scope, name)
let reference_of_name globals_are_properties bindings name =
  if StringMap.mem name bindings then
    StringMap.find name bindings
  else if globals_are_properties then
    Field(global_object, name)
  else
    Variable(Global, name)

module VersionedReference = Pairs.Make(Reference)(struct include BatInt let pp = Fmt.int end)
type versioned_reference = VersionedReference.t
let pp_versioned_reference = VersionedReference.pp

module VersionedReferenceMap = ExtMap.Make(VersionedReference);;
module VersionedReferenceSet = Set.Make(VersionedReference);;
let pp_versioned_reference_map fmtval =
  VersionedReferenceMap.pp fmtval
let pp_versioned_reference_set =
  let open Fmt in
    using VersionedReferenceSet.elements (list pp_versioned_reference)

type points_to_map = Types.jsval VersionedReferenceMap.t

let pp_points_to_map = pp_versioned_reference_map Types.pp_jsval


