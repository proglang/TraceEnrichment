(** Calculation of rich traces *)
open TraceTypes
(** Transform a trace file with facts to a rich trace file. *)
val calculate_rich_tracefile : facts_tracefile -> rich_tracefile
(** Transform a trace stream with facts to a rich trace stream. *)
val calculate_rich_stream : Types.initials -> facts_stream -> rich_stream

(** Transform a raw trace file to a rich trace file. *)
val tracefile_to_rich_tracefile : tracefile -> rich_tracefile
(** Transform a raw trace stream to a rich trace stream. *)
val trace_stream_to_rich_stream : Types.initials -> raw_stream -> rich_stream
