open Types
open TraceTypes
(** [parse_tracefile c] parses a JSON trace file from input channel [c] and returns it. *)
val parse_tracefile : in_channel -> tracefile
(** Parse from JSON string *)
val event_of_string: string -> event
val objectspec_of_string: string -> objectspec
val funcspec_of_string: string -> funcspec
val jsval_of_string: string -> jsval

(** [format_tracefile c t] formats a trace file [c] as JSON and writes it to the
  * output channel [c]. *)
val format_tracefile : out_channel -> tracefile -> unit
val format_clean_tracefile : out_channel -> clean_tracefile -> unit
val format_rich_tracefile : out_channel -> rich_tracefile -> unit
val format_rich_tracefile_small : out_channel -> rich_tracefile -> unit
(** Format as JSON string *)
val string_of_event: event -> string
val string_of_objectspec: objectspec -> string
val string_of_funcspec: funcspec -> string
val string_of_jsval: jsval -> string
val string_of_clean_event : clean_operation -> string
val string_of_rich_operation : rich_operation -> string
val string_of_rich_event : rich_event -> string
val string_of_rich_event_small : rich_event -> string
val string_of_points_to : Reference.points_to_map -> string


