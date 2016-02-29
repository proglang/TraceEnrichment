open LocalFacts
open TraceTypes
open Types
open Reference

module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_operation * LocalFacts.versions_resolved) trace ->
    (clean_operation * LocalFacts.local_facts) trace
end
module Make(T: Streaming.Transformers):
              S with type 'a trace = 'a T.sequence

(** Calculate an initial points-to map for the given initials. *)
val initial_pointsto : initials -> points_to_map
