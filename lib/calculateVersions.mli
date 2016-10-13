open TypesJS
open TraceTypes
(** Fill in the [aliases] and [version] fields of the given
 trace file. Note that this requires the arguments facts to be filled in. *)
module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_event * LocalFacts.prototypes_resolved) trace ->
    (clean_event * LocalFacts.versions_resolved) trace
end
module Make(T: Streaming.Transformers): S with type 'a trace = 'a T.sequence

(** [initial_versions objs globals globals_are_properties] calculates
 an initial version map for the given globals. *)
val initial_versions: objects -> globals -> bool -> int Reference.ReferenceMap.t
