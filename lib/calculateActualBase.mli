open TypesJS
open TraceTypes
(** Fill in the [aliases] and [version] fields of the given
 trace file. Note that this requires the arguments facts to be filled in. *)
module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_operation * LocalFacts.prototypes_resolved) trace ->
    (clean_operation * LocalFacts.prototypes_resolved) trace
end
module Make(T: Streaming.Transformers): S with type 'a trace = 'a T.sequence
