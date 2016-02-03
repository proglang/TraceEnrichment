open Types
(** [find_object_facts id facts pt] finds all points-to facts
 * for object [id], using [facts] to find versions for all fields and [pt] to find
 * the corresponding values. *)
val find_object_facts: objectid -> TraceTypes.local_facts -> Reference.points_to_map -> jsval StringMap.t
