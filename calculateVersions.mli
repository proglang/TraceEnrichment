open Types
open TraceTypes
(** This function fills in the [aliases] and [version] fields of the given
 * trace file. Note that this requires the arguments facts to be filled in. *)
val collect_versions_trace: arguments_tracefile -> facts_tracefile
val collect_versions_stream: Reference.initials -> arguments_stream -> facts_stream
val initial_versions: objects -> globals -> bool -> int Reference.ReferenceMap.t
