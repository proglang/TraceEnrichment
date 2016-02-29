(** Parsing and formatting of traces. *)
open Types
open TraceTypes
(** [parse_tracefile c] parses a JSON trace file from input channel [c] and returns it. *)
val parse_tracefile : in_channel -> tracefile
(** Parse from JSON string or JSON object. *)
val event_of_string: string -> event
val objectspec_of_string: string -> objectspec
val funcspec_of_string: string -> funcspec
val jsval_of_string: string -> jsval
val parse_jsval: Yojson.Basic.json -> jsval
val parse_funcspec: Yojson.Basic.json -> funcspec
val parse_objectspec: Yojson.Basic.json -> objectspec
val parse_operation: Yojson.Basic.json -> event


