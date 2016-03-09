(** {1 References to mutable state} *)

(** {2 The scope of a variable} *)
type scope = Global | Local of int
(** Boilerplate code. *)
val pp_scope : Format.formatter -> scope -> Ppx_deriving_runtime.unit
val compare_scope : scope -> scope -> Ppx_deriving_runtime.int
val equal_scope : scope -> scope -> Ppx_deriving_runtime.bool

(** {2 A reference to mutable state}

 Because of aliasing issues, the transformation from variables to
 references is somewhat tricky. It is recommended to use the helpers below. *)
type reference = Field of Types.fieldref | Variable of scope * string
(** Boilerplate code. *)
val pp_reference : reference Fmt.t
val equal_reference : reference -> reference -> bool

(** {3 Reference constructors} *)
(** Transform a field access to a reference.

 Call as [reference_of_field base offset], where [base] must be a
 value having an object identifier. *)
val reference_of_field : Types.jsval -> string -> reference
(** Transform a field reference to a reference. *)
val reference_of_fieldref : Types.fieldref -> reference
(** Transform a variable name that is known to be local and non-aliased to
 a reference.

 Beware: Unless you are certain that no aliasing occurs for this name,
 use [reference_of_name]! *)
val reference_of_local_name : int -> string -> reference
(** Create a reference for a variable name.

  [reference_of_name globals_are_properties name_map name] creates
  the appropriate reference for [name] using the map [name_map] of known
  variables, and [globals_are_properties] to create the appropriate reference
  for as-yet-unknown global variables. *)
val reference_of_name : bool -> reference StringMap.t -> string -> reference

(** Map with references as keys. *)
module ReferenceMap : ExtMap.S with type key = reference
val pp_reference_map :
  ?pair_sep:unit Fmt.t ->
  ?entry_sep:unit Fmt.t ->
  ?entry_frame:((ReferenceMap.key * 'b) Fmt.t ->
                (ReferenceMap.key * 'b) Fmt.t) ->
  'b Fmt.t -> 'b ReferenceMap.t Fmt.t

(** {2 A pair of reference and version}

   These pairs provide references to different versions of a reference. *)
type versioned_reference = reference * int
val pp_versioned_reference : versioned_reference Fmt.t

(** Maps and sets of versioned references. *)
module VersionedReferenceMap : CCPersistentHashtbl.S with type key = versioned_reference
module VersionedReferenceSet : Set.S with type elt = versioned_reference
val pp_versioned_reference_map : 'a Fmt.t -> 'a VersionedReferenceMap.t Fmt.t
val pp_versioned_reference_set : VersionedReferenceSet.t Fmt.t
type points_to_map = Types.jsval VersionedReferenceMap.t
val pp_points_to_map : points_to_map Fmt.t
