open Types
open TraceTypes
open Streaming

module CleanGeneric(S: Streaming.Transformers): sig
  val calculate_clean_trace: initials -> TraceTypes.event S.sequence ->
    TraceTypes.clean_operation S.sequence
end

(** Synthesize events for external functions. This is exposed for testing purposes. *)
val synthesize_events: functions -> clean_trace -> clean_trace

(** Transform a trace file to a clean trace file. *)
val clean_tracefile: tracefile -> clean_tracefile
(** Transform a trace stream to a clean trace stream. *)
val clean_stream: initials -> event Stream.t -> clean_operation Stream.t
