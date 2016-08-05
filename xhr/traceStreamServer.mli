(** A server that handles tracing streams. *)

(** Short-hand for specifying handlers. *)
type 'a handler_spec = ((string * Cohttp.Code.meth) * (string * 'a)) list
(** A module typing describing strategies for handling event streams. *)
module type STREAMSTRATEGY =
  sig
    (** Internal type for stream handling. *)
    type t

    (** [stream_setup id initials stream] sets up handling of an event stream,
      given a unique id, an initial state and an event stream. *)
    val stream_setup : string -> TypesJS.initials -> TraceTypes.raw_stream -> t

    (** Global handlers to provide by the server. Each entry is of the form
        [((path, method), (desc, handler))], [path] and [method] are used to
        construct the URL under which the handler can be reached,
        [desc] is a human-readable description of the method, and
        [handler] is a function to handle the request, taking the URI of the
        request and the request body as arguments and returning a CohTTP response. *)
    val handlers_global : (TraceCollector.handler) handler_spec
    (** Local handlers to provide by the server. Each entry is of the form
        [((path, method), (desc, handler))], [path] and [method] are used to
        construct the URL under which the handler can be reached,
        [desc] is a human-readable description of the method, and
        [handler] is a function to handle the request, taking
        the stream data, the URI of the request and the request body as
        arguments and returning a CoHTTP response. *)
    val handlers_local : (t -> TraceCollector.handler) handler_spec
  end

(** Interface of the server. *)
module type TSS =
  sig
    (** The server implementation constructor. *)
    val server : unit -> unit Lwt.t
    (** A simple interface for running just the server. *)
    val run_server : unit -> unit
  end
(** Functor for creating a stream server. *)
module TraceStreamServer : functor (S : STREAMSTRATEGY) -> TSS
