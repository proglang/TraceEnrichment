type 'a handler_spec = ((string * Cohttp.Code.meth) * (string * 'a)) list
module type STREAMSTRATEGY = sig
  open TraceCollector

  type t
  val stream_setup: string -> TypesJS.initials -> TraceTypes.raw_stream -> t
  val handlers_global: handler handler_spec
  val handlers_local: (t -> handler) handler_spec
end

module type TSS = sig
  val server: unit -> unit Lwt.t
  val run_server: unit -> unit
end

module TraceStreamServer(S: STREAMSTRATEGY): TSS = struct
  module Strategy = struct

    let sinks = Hashtbl.create 20

    let make_trace_sink ~init_data ~id ~finish =
      let (initials, stream, wakeup, sink) =
        TraceStream.parse_setup_packet init_data in
      let sink_data =
        Lwt.map (fun () -> S.stream_setup id initials stream) wakeup
      in
        Hashtbl.add sinks id sink_data;
        Log.debug (fun m -> m "Created new trace sink with ID %s for %s"
                              id init_data);
        Lwt.return sink

    let handler_map (fn: string -> S.t -> TraceCollector.handler) id uri body =
      let sink = Hashtbl.find sinks id
      in Lwt.bind sink (fun data -> fn data uri body)

    let handlers_global = S.handlers_global
    let handlers_local =
      List.map (fun (key, (name, fn)) ->
                  (key, (name, handler_map fn)))
        S.handlers_local
  end
  include TraceCollector.Server(Strategy)
  let run_server () =
    let usage = Sys.argv.(0) ^ " [options]" in
    Arg.parse Config.args (fun _ -> Arg.usage Config.args usage; exit 1) usage;
    Log.default_setup true;
    Log.info (fun m -> m "Setting up server");
    Config.setup();
    Log.info (fun m -> m "Starting server");
    Lwt_main.run (server ())
end

