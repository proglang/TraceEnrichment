module ToRich(S: Streaming.Transformers): sig
  open TraceTypes
  val enriched_trace_to_rich_trace:
    bool -> (clean_event * LocalFacts.local_facts) S.sequence ->
    TraceTypes.rich_event S.sequence
  val trace_to_rich_trace:
      TypesJS.initials -> event S.sequence -> TraceTypes.rich_event S.sequence
end

(** Calculation of rich traces *)
open TraceTypes
(** Transform a trace file with facts to a rich trace file. *)
val calculate_rich_tracefile : LocalFacts.local_facts enriched_tracefile -> rich_tracefile
(** Transform a trace stream with facts to a rich trace stream. *)
val calculate_rich_stream : TypesJS.initials -> LocalFacts.local_facts enriched_stream -> rich_stream

(** Transform a raw trace file to a rich trace file. *)
val tracefile_to_rich_tracefile : tracefile -> rich_tracefile
(** Transform a raw trace stream to a rich trace stream. *)
val trace_stream_to_rich_stream : TypesJS.initials -> raw_stream -> rich_stream

(** A dummy rich_fact instance; this can be helpful in further analyses. *)
val dummy_facts: rich_facts

(** Debugging: disable TSID calculation *)
val calculate_tsid_flag: bool ref
