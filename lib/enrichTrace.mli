module Make(T : Streaming.Transformers):
sig
  val collect :
    TypesJS.initials ->
    TraceTypes.clean_operation T.sequence ->
    (TraceTypes.clean_operation * LocalFacts.local_facts) T.sequence
end
