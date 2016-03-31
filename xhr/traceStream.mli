exception InvalidItem of string
val parse_setup_packet :
  string ->
  Types.initials * TraceTypes.event Lwt_stream.t * (string -> unit)
