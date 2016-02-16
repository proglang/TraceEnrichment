open Kaputt
open TraceCollector

module TestStrategy = struct
  type event =
      MakeSink of string * string
    | Facts of string
    | Query of string * string
    | LocalQuery of string * string * string
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
    reply_text "application/binary" (Marshal.to_string !events [])
  let events_parse str = (Marshal.from_string str 0: event list)

  open Cohttp.Code
  let handlers_global =
    [ (("query", `GET), query_handler);
      (("events", `GET), events_handler) ]
  let handlers_local = [ (("query", `GET), local_query_handler) ]
end

module TestServer = Server(TestStrategy)

let setup_server () =
  match Unix.fork () with
    | 0 -> Lwt_main.run TestServer.server; exit 0
    | pid -> pid
let shutdown_server pid =
  Unix.kill pid Sys.sigint;
  Unix.kill pid Sys.sigkill

let server_test title test =
  Test.make_assert_test ~title setup_server
    (fun pid -> Lwt_main.run (test ()); pid) shutdown_server

let get path =
  Cohttp_lwt_unix.Client.get (Uri.of_string ("http://localhost:8888/" ^ path))
let expect_get path check_body =
  let open Cohttp.Code in
  let%lwt (response, body) = get path in
    Assertion.make_equal (=) string_of_status `OK (Cohttp.Response.status response);
    check_body body;
    Lwt.return_unit
let expect_get_success path = expect_get path (fun _ -> ())
let expect_events expected =
  expect_get "events"
    (fun got ->
       let open TestStrategy in
       let%lwt got = Cohttp_lwt_body.to_string got in
       let got = events_parse got in
         Assertion.make_equal_list (=)
           (function
                MakeSink (id, init) -> "mksink: id=" ^ id ^ ", init=" ^ init
              | Facts data -> "facts: " ^ data
              | Query (url, body) -> "query: " ^ url ^ " with " ^ body
              | LocalQuery (id, url, body) -> "query(" ^ id ^ "): " ^ url ^ " with " ^ body)
           expected got;
         Lwt.return_unit)

open  TestStrategy
let test_setup_and_shutdown =
  server_test "setup and shutdown"
    (fun () ->
       expect_get_success "query" >>
       expect_get_success "query" >>
       expect_events [ Query ("", ""); Query ("", "") ])

let _ = Test.run_tests [test_setup_and_shutdown]
