(** Parser for trace streaming events. *)

(** An exception indicating that received data was incorrect. *)
exception InvalidItem of string

(** Parse a trace setup packet (given as a string), returing
    four values:
    - An [initials] object, containing information about the state
      of the JavaScript interpreter when analysis started,
    - A stream of trace events,
    - A LWT thread wakener that is invoked once the "start" packet is received,
    - A function that takes further trace packets (given as strings)
      and feeds them to the stream.
*)
val parse_setup_packet :
  string ->
  TypesJS.initials * TraceTypes.event Lwt_stream.t * unit Lwt.t * (string -> unit)
