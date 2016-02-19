exception InvalidItem
val parse_setup_packet :
  string ->
  Types.initials * TraceTypes.event Lwt_stream.t * (string -> unit)
