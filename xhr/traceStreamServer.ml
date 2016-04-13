module type STREAMSTRATEGY = sig
  open TraceCollector

  type t

  val stream_setup: string -> Types.initials -> TraceTypes.raw_stream -> t
  val handlers_global: ((string * Cohttp.Code.meth) * (string * handler)) list
  val handlers_local: ((string * Cohttp.Code.meth) * (string * (t -> handler))) list
end

module type TSS = sig
  val server: unit -> unit Lwt.t
  val run_server: unit -> unit
end

module TraceStreamServer(S: STREAMSTRATEGY): TSS = struct
  module Strategy = struct

    let sinks = Hashtbl.create 20

    let make_trace_sink ~init_data ~id ~finish =
      let (initials, stream, sink) =
        TraceStream.parse_setup_packet init_data in
      let sink_data = S.stream_setup id initials stream in
        Hashtbl.add sinks id sink_data;
        Log.debug (fun m -> m "Created new trace sink with ID %s for %s"
                              id init_data);
        Lwt.return sink

    let handlers_global = S.handlers_global
    let handlers_local =
      List.map (fun (key, (name, fn)) ->
                  (key, (name, fun id -> fn (Hashtbl.find sinks id))))
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

