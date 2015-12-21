open Types
open TraceTypes
open Streaming

(** Transform a trace file to a clean trace file. *)
val clean_tracefile: tracefile -> clean_tracefile
val clean_stream: Reference.initials -> event Stream.t -> clean_operation Stream.t
