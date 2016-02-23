open TraceCollector

module TestStrategy = struct
  type event =
      MakeSink of string * string
    | Facts of string
    | Query of string * string
    | LocalQuery of string * string * string
    [@@deriving yojson]

  let events = ref []
  let init () = events := []
  let push new_events = events := !events @ new_events

  let make_trace_sink ~init_data ~id ~finish =
    push [ MakeSink(init_data, id) ];
    Lwt.return (fun str -> push [ Facts str ])
  let query_handler uri body =
    let%lwt body = body in
      push [ Query (Uri.path uri, body) ];
      reply_plain_text "query performed"
  let local_query_handler id uri body =
    let%lwt body = body in
      push [ LocalQuery (id, Uri.path uri, body) ];
      reply_plain_text "local query performed"

  let events_handler uri body =
    let open Yojson.Safe in
    let evs = !events in
      events := [];
      reply_text "application/json"
        (`List (List.map event_to_yojson evs) |> to_string)

  open Cohttp.Code
  let handlers_global =
    [ (("query", `GET), ("Debugging query", query_handler));
      (("events", `GET), ("List of events", events_handler)) ]
  let handlers_local = [ (("query", `GET), ("Debugging query", local_query_handler)) ]
end

module TestServer = Server(TestStrategy)

let _ =
  Arg.parse Config.args (fun _ -> prerr_endline "No arguments expected")
    "Usage: traceCollectorTest [options]\n\
     Run a testing version of TraceCollector";
  Lwt_main.run(TestServer.server)
