module type S =
  sig
    type 'a trace
    val collect :
      Types.initials ->
      (TraceTypes.clean_operation * LocalFacts.arguments_and_closures) trace ->
      (TraceTypes.clean_operation * LocalFacts.names_resolved) trace
  end
module Make :
  functor (T : Streaming.Transformers) -> S with type 'a trace = 'a T.sequence
