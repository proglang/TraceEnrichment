module TestStreamStrategy: TraceStreamServer.STREAMSTRATEGY = struct
  open Lwt
  type t = string list Lwt.t
  let stream_setup id initials raw_stream =
    Log.info (fun m -> m "Setting up stream");
    (CleanTrace.clean_stream initials raw_stream |>
      Lwt_stream.to_list) >|=
      List.map (fun ev -> Fmt.to_to_string TraceTypes.pp_clean_event ev)

  let stream_dump _ (strings: t) _ _ =
    Log.info (fun m -> m "Preparing for dump");
    Lwt.bind strings
    (fun strings ->
       TraceCollector.reply_plain_text (BatString.concat "\n" strings))

  let handlers_global = []
  let handlers_local = [
    ("pretty", let open Cohttp.Code in `GET),
    ("Pretty-print trace", stream_dump)
  ]
end

module Server = TraceStreamServer.TraceStreamServer(TestStreamStrategy)

let () =
  Arg.parse Config.args (fun _ -> failwith "Unexpected argument")
    "traceStreamServerTest [options]";
  Log.default_setup true;
  Server.run_server ()

