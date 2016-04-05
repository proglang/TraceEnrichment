open Types
open TraceTypes

(** Facts that are local to a position in the trace. *)
type arguments_and_closures = {
  (** The last argument object that was created by a function call. *)
  last_arguments: int option;
  (** Closure enviroments for functions. *)
  closures: int IntMap.t
}
val pp_arguments_and_closures: Format.formatter -> arguments_and_closures -> unit
type names_resolved = {
  (** The last argument object that was created by a function call. *)
  last_arguments: int option;
  (** Closure enviroments for functions. *)
  closures: Reference.reference StringMap.t IntMap.t;
  (** All visible variable names. *)
  names: Reference.reference StringMap.t
}
val pp_names_resolved: Format.formatter -> names_resolved -> unit
type versions_resolved = {
  (** The last argument object that was created by a function call. *)
  last_arguments: int option;
  (** Closure enviroments for functions. *)
  closures: Reference.reference StringMap.t IntMap.t;
  (** The last reference that was modified. *)
  last_update: Reference.versioned_reference option;
  (** The current version of all known references. *)
  versions: int Reference.ReferenceMap.t;
  (** All visible variable names. *)
  names: Reference.reference StringMap.t;
  (** Fresh versioned references from the last step. *)
  fresh_versioned_references: Reference.versioned_reference list
}
val pp_versions_resolved: Format.formatter -> versions_resolved -> unit
type local_facts = {
  (** The last argument object that was created by a function call. *)
  last_arguments: int option;
  (** Closure enviroments for functions. *)
  closures: Reference.reference StringMap.t IntMap.t;
  (** The last reference that was modified. *)
  last_update: Reference.versioned_reference option;
  (** The current version of all known references. *)
  versions: int Reference.ReferenceMap.t;
  (** All visible variable names. *)
  names: Reference.reference StringMap.t;
  (** The current state of the points-to map. *)
  points_to: Reference.points_to_map
}
val pp_local_facts: Format.formatter -> local_facts -> unit

(** Create a reference for a variable.

 [reference_of_variable globals_are_properties facts name]
 creates the appropriate reference for the variable called [name],
 taking all relevant facts into account. Note that this requires
 the [names] field of [facts] to be filled in. *)
val reference_of_variable: bool -> local_facts -> string -> Reference.reference

(** Turn a reference into a versioned reference using the current
 version from the given facts. Note that this required the [versions] field
 to be filled in. *)
val make_versioned_impl : int Reference.ReferenceMap.t -> Reference.reference -> Reference.versioned_reference
val make_versioned : rich_facts -> Reference.reference -> Reference.versioned_reference

module CollectArguments(S: Streaming.Transformers) : sig
  val collect: clean_operation S.sequence -> (clean_operation * int option) S.sequence
end
module CollectClosures(S: Streaming.Transformers) : sig
  val collect: (clean_operation * int option) S.sequence ->
    (clean_operation * arguments_and_closures) S.sequence
end

(** Object bound for filtered pretty-printing. *)
val filter_bound: int ref

