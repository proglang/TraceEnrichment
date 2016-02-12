open LocalFacts
open TraceTypes
open Types
open Reference

(** Calculate a points-to map for a given trace file. Note that this requires
 the version map to be initialized. *)

(** Calculate an initial points-to map for the given initials. *)
val initial_pointsto : initials -> points_to_map
(** Calculate a points-to map for the given trace file with facts. *)
val calculate_pointsto : facts_tracefile -> full_facts_tracefile
(** Calculate a points-to map for the given trace stream with facts. *)
val collect_pointsto_stream : initials -> facts_stream -> full_facts_stream
