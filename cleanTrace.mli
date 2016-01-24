open Types
open TraceTypes
open Streaming

(** For testing. *)
val synthesize_events: functions -> clean_trace -> clean_trace

(** Transform a trace file to a clean trace file. *)
val clean_tracefile: tracefile -> clean_tracefile
val clean_stream: Reference.initials -> event Stream.t -> clean_operation Stream.t
