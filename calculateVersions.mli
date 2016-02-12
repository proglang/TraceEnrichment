open Types
open TraceTypes
(** Fill in the [aliases] and [version] fields of the given
 trace file. Note that this requires the arguments facts to be filled in. *)
(** [initial_versions objs globals globals_are_properties] calculates
 an initial version map for the given globals. *)
val initial_versions: objects -> globals -> bool -> int Reference.ReferenceMap.t
(** Calculate globals and aliases for the given trace file (with arguments) *)
val collect_versions_trace: arguments_tracefile -> facts_tracefile
(** Calculate globals and aliases for the given trace stream (with arguments) *)
val collect_versions_stream: initials -> arguments_stream -> facts_stream
