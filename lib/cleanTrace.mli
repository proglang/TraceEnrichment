open Types
open TraceTypes
open Streaming

(** Synthesize events for external functions. This is exposed for testing purposes. *)
val synthesize_events: functions -> clean_trace -> clean_trace

(** Transform a trace file to a clean trace file. *)
val clean_tracefile: tracefile -> clean_tracefile
(** Transform a trace stream to a clean trace stream. *)
val clean_stream: initials -> event Stream.t -> clean_operation Stream.t
