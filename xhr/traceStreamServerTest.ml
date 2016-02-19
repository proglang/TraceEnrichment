module TestStreamStrategy: TraceStreamServer.STREAMSTRATEGY = struct
  open Lwt
  type t = string list Lwt.t
  let stream_setup id initials raw_stream =
    Lwt_stream.to_list raw_stream >|=
      List.map (fun ev -> Fmt.to_to_string TraceTypes.pp_operation ev)

  let stream_dump (strings: t) _ _ =
    Lwt.bind strings
    (fun strings -> TraceCollector.reply_plain_text (BatString.concat "\n" strings))

  let display_menu _ _ =
    let headers = Cohttp.Header.init_with "Content-Type" "text/html" in
    Cohttp_lwt_unix.Server.respond_file ~headers ~fname:"menu.html" ()

  let handlers_global = [
    ("menu", let open Cohttp.Code in `GET),
    display_menu
  ]
  let handlers_local = [
    ("pretty", let open Cohttp.Code in `GET),
    stream_dump
  ]
end

module Server = TraceStreamServer.TraceStreamServer(TestStreamStrategy)

let () =
  Arg.parse Config.args (fun _ -> failwith "Unexpected argument")
    "traceStreamServerTest [options]";
  Server.run_server ()

