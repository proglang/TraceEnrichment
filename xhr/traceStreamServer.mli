module type STREAMSTRATEGY =
  sig
    type t
    val stream_setup : string -> Types.initials -> TraceTypes.raw_stream -> t
    val handlers_global :
      ((string * Cohttp.Code.meth) * (string * TraceCollector.handler)) list
    val handlers_local :
      ((string * Cohttp.Code.meth) * (string * (t -> TraceCollector.handler)))
      list
  end
module type TSS =
  sig
    val server : unit -> unit Lwt.t
    val run_server : unit -> unit
  end
module TraceStreamServer : functor (S : STREAMSTRATEGY) -> TSS
