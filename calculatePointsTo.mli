open LocalFacts
open Reference
open TraceTypes

(** Calculate a points-to map for a given trace file. Note that this requires
 * the version map to be initialized. *)
val calculate_pointsto : points_to_map ref -> facts_tracefile -> facts_tracefile
val collect_pointsto_stream : initials -> facts_stream -> facts_stream
