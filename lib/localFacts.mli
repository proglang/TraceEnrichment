open Types
open TraceTypes

(** Local facts are facts that change depending on the position
 in the trace. *)

(** Transform a clean trace into a trace with arguments information. *)
val collect_arguments_trace: clean_trace -> arguments_trace
(** Transform a clean trace file into a trace file with arguments information. *)
val collect_arguments_tracefile: clean_tracefile -> arguments_tracefile
(** Transform a clean trace stream into a trace stream with arguments information. *)
val collect_arguments_stream: clean_stream -> arguments_stream

(** Create a reference for a variable.

 [reference_of_variable globals_are_properties facts name]
 creates the appropriate reference for the variable called [name],
 taking all relevant facts into account. Note that this requires
 the [names] field of [facts] to be filled in. *)
val reference_of_variable: bool -> local_facts -> string -> Reference.reference

(** Turn a reference into a versioned reference using the current
 version from the given facts. Note that this required the [versions] field
 to be filled in. *)
val make_versioned : local_facts -> Reference.reference -> Reference.versioned_reference

