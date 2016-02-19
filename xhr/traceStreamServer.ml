module type STREAMSTRATEGY = sig
  open TraceCollector

  type t

  val stream_setup: string -> Types.initials -> TraceTypes.raw_stream -> t Lwt.t
  val handlers_global: ((string * Cohttp.Code.meth) * handler) list
  val handlers_local: ((string * Cohttp.Code.meth) * (t -> handler)) list
end

module type TSS = sig
  val server: unit Lwt.t
  val run_server: unit -> unit
end

module TraceStreamServer(S: STREAMSTRATEGY): TSS = struct
  module Strategy = struct

    let sinks = Hashtbl.create 20

    let make_trace_sink ~init_data ~id ~finish =
      let (initials, stream, sink) =
        TraceStream.parse_setup_packet init_data in
      let%lwt sink_data = S.stream_setup id initials stream in
        Hashtbl.add sinks id sink_data;
        Lwt.return sink

    let handlers_global = S.handlers_global
    let handlers_local =
      List.map (fun (key, fn) ->
                  (key, fun id -> fn (Hashtbl.find sinks id)))
        S.handlers_local
  end
  include TraceCollector.Server(Strategy)
  let run_server () = Lwt_main.run server
end

