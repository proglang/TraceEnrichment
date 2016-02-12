open LocalFacts
open TraceTypes
open Types
open Reference

(** Calculate a points-to map for a given trace file. Note that this requires
 * the version map to be initialized. *)
val initial_pointsto : initials -> points_to_map
val calculate_pointsto : facts_tracefile -> full_facts_tracefile
val collect_pointsto_stream : initials -> facts_stream -> full_facts_stream
