type scope =
  | Global
  | Local of int
  [@@deriving show, ord, eq]

module Reference = struct
  type t =
    | Field of Types.objectid * string
    | Variable of scope * string
    [@@deriving show, ord, eq]
end
open Reference
type reference = Reference.t
let pp_reference = Reference.pp
let pp_equal = Reference.equal

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
let pp_reference_map fmtval = ReferenceMap.pp

let global_object = Types.Object 0

let reference_of_field base offset = Field (Types.objectid_of_jsval base, offset)
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


