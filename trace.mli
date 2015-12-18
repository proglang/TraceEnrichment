open Types
open TraceTypes
(** [parse_tracefile c] parses a JSON trace file from input channel [c] and returns it. *)
val parse_tracefile : in_channel -> tracefile
(** Parse from string *)
val event_of_string: string -> event
val objectspec_of_string: string -> objectspec
val funcspec_of_string: string -> funcspec
val jsval_of_string: string -> jsval
