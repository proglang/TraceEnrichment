module Make(T : Streaming.Transformers):
sig
  val collect :
    TypesJS.initials ->
    TraceTypes.clean_event T.sequence ->
    (TraceTypes.clean_event * LocalFacts.local_facts) T.sequence
end
