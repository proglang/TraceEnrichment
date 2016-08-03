(** Parser for trace streaming events. *)

(** An exception indicating that received data was incorrect. *)
exception InvalidItem of string

(** Parse a trace setup packet (given as a string), returing
    three things:
    - An [initials] object, containing information about the state
      of the JavaScript interpreter when analysis started,
    - A stream of trace events,
    - A function that takes further trace packets (given as strings)
      and feeds them to the stream.
*)
val parse_setup_packet :
  string ->
  Types.initials * TraceTypes.event Lwt_stream.t * (string -> unit)
