exception InvalidItem
val parse_setup_packet :
  string ->
  Reference.initials * TraceTypes.event Lwt_stream.t * (string -> unit)
