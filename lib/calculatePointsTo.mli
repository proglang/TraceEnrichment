open LocalFacts
open TraceTypes
open TypesJS
open Reference

module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_event * LocalFacts.versions_resolved) trace ->
    (clean_event * LocalFacts.local_facts) trace
end
module Make(T: Streaming.Transformers):
              S with type 'a trace = 'a T.sequence

(** Calculate an initial points-to map for the given initials. *)
val initial_pointsto : initials -> points_to_map
