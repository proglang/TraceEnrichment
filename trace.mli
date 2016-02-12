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

(** [format_tracefile c t] formats a trace file [c] as JSON and writes it to the
  * output channel [c]. *)
val format_tracefile : out_channel -> tracefile -> unit
val format_clean_tracefile : out_channel -> clean_tracefile -> unit
val format_rich_tracefile : out_channel -> rich_tracefile -> unit
val format_rich_tracefile_small : out_channel -> rich_tracefile -> unit
(** Format as JSON string. *)
val string_of_event: event -> string
val string_of_objectspec: objectspec -> string
val string_of_funcspec: funcspec -> string
val string_of_jsval: jsval -> string
val string_of_clean_event : clean_operation -> string
val string_of_rich_operation : rich_operation -> string
val string_of_rich_event : rich_event -> string
val string_of_rich_event_small : rich_event -> string
val string_of_points_to : Reference.points_to_map -> string

(** Format as JSON object. *)
val format_jsval : jsval -> Yojson.Basic.json
val format_fieldspec : fieldspec -> Yojson.Basic.json
val format_objectspec : objectspec -> Yojson.Basic.json
val format_objects : objects -> Yojson.Basic.json
val format_funcspec : funcspec -> Yojson.Basic.json
val format_functions : functions -> Yojson.Basic.json
val format_globals : globals -> Yojson.Basic.json
val format_objectid : objectid -> Yojson.Basic.json
val format_fieldref : fieldref -> Yojson.Basic.json
val format_event : event -> Yojson.Basic.json
val format_trace : trace -> Yojson.Basic.json
val format_clean_event : clean_operation -> Yojson.Basic.json
val format_reference : Reference.reference -> Yojson.Basic.json
val format_versioned_reference : Reference.versioned_reference -> Yojson.Basic.json
val format_rich_operation : rich_operation -> Yojson.Basic.json
val format_rich_event : rich_event -> Yojson.Basic.json
val format_rich_event_small : rich_event -> Yojson.Basic.json
val format_points_to : Reference.points_to_map -> Yojson.Basic.json
