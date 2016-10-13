module type S =
  sig
    type 'a trace
    val collect :
      TypesJS.initials ->
      (TraceTypes.clean_event * LocalFacts.arguments_and_closures) trace ->
      (TraceTypes.clean_event * LocalFacts.names_resolved) trace
  end
module Make :
  functor (T : Streaming.Transformers) -> S with type 'a trace = 'a T.sequence

exception ClosureEnvNotFound of
  int * int * Reference.reference StringMap.t IntMap.t


