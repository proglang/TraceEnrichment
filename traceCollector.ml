let (/:) = Filename.concat

let instrument_script_path =
  List.fold_right (/:) [ "src"; "js"; "commands" ] "generic-tracer.js"

exception InstrumentationError

let instrument filename outdir =
  let open Lwt in
  let argarray =
    [| "node";
       Config.get_jalangi2_path() /: instrument_script_path;
       "--inlineIID";
       "--inlineSource";
       "--analysis"; Config.get_analysis_script_path ();
       "--initParam"; "host:" ^ (Config.get_xhr_server_address ());
       "--initParam"; "port:" ^ (string_of_int (Config.get_xhr_server_port ()));
       "--outputDir"; outdir;
       filename |]
  in match Unix.fork () with
    | 0 -> Unix.execv (Config.get_node_path ()) argarray
    | pid ->
        Lwt_unix.waitpid [] pid
          >|= function
            | (_, Unix.WEXITED 0) -> ()
            | _ -> raise InstrumentationError

let sinks: (string, string -> unit) Hashtbl.t = Hashtbl.create 20

let handle_new make_trace_sink uri body =
  let%lwt init_json = Cohttp_lwt_body.to_string body in
  let id = Uuidm.v4_gen (Random.get_state ()) () |> Uuidm.to_string in
  match%lwt make_trace_sink ~init_data:init_json ~id:id with
    | Some sink ->
          Hashtbl.add sinks id sink;
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:id ()
    | None ->
        Cohttp_lwt_unix.Server.respond_error
          ~status:`Forbidden ~body:"No trace sinks available" ()

let handle_facts make_trace_sink uri body =
  let open Lwt in
  match Uri.get_query_param uri "id" with
    | Some id ->
        begin try
          let sink = Hashtbl.find sinks id in
            Cohttp_lwt_body.to_string body >>=
              fun body -> sink body;
                          Cohttp_lwt_unix.Server.respond_string ~status:`Accepted
                            ~body:"" ()
        with
            Not_found ->
              Cohttp_lwt_unix.Server.respond_error ~status:`Not_found
                ~body:("Trace pipeline for " ^ id ^ " not found") ()
        end
    | _ ->
        Cohttp_lwt_unix.Server.respond_error ~status:`Bad_request
          ~body:"No trace id given" ()

let instrument_tmp_dir = ref None

let get_instrument_tmp_dir () =
  match !instrument_tmp_dir with
    | Some dir -> (dir, dir /: "instrumented")
    | None ->
        let path = Filename.temp_file "jstools" "" in
          Unix.mkdir path 0o700;
          Unix.mkdir (path /: "instrumented") 0o700;
          instrument_tmp_dir := Some path;
          (path, path /: "instrumented")

let make_html_wrapper script_name =
  Fmt.strf
    "<html><head><script link=\"instrumented/%s\"/></head><body>Running...</body></html>"
    script_name

let handle_instrument make_trace_sink uri body =
  let (tmpdir, insdir) = get_instrument_tmp_dir () in
  let filename = match Uri.get_query_param uri "filename" with
    | Some filename -> tmpdir /: Filename.basename filename
    | None -> Filename.temp_file ~temp_dir:tmpdir "gen" ".js"
  in if Sys.file_exists filename then
    Cohttp_lwt_unix.Server.respond_error ~status:`Conflict
      ~body:("JS file called " ^ filename ^ " already exists") ()
  else
    let outchan = open_out filename in
    let%lwt _ = Cohttp_lwt_body.write_body
                  (fun str -> Lwt.return (output_string outchan str))
                  body
    in
      close_out outchan;
      let%lwt _ = instrument filename insdir in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK
          ~body:(make_html_wrapper (Filename.basename filename)) ()

let trace_connection_switch_tab =
  [ ("/new", [ (`POST, handle_new) ]);
    ("/facts", [ (`POST, handle_facts) ]);
    ("/instrument", [ (`POST, handle_instrument) ])
  ]

let slash_re = Str.regexp "/"

let trace_collection_handler make_trace_sink conn req body =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let uri = Request.uri req
  and meth = Request.meth req in
    try
      (* Try the regular handlers first. *)
      let handlers = List.assoc (Uri.path uri) trace_connection_switch_tab in
        try
          (List.assoc meth handlers) make_trace_sink uri body
        with Not_found -> Server.respond_error ~status:`Method_not_allowed ~body:"" ()
    with Not_found ->
      (* Try to find an instrumented script. *)
      match Str.split slash_re (Uri.path uri) with
        | [ "instrumented"; filename ] ->
            let filename = Filename.basename filename in
            let (_, insdir) = get_instrument_tmp_dir () in
            let path = insdir /: filename in
              if Sys.file_exists path then
                Server.respond_file ~fname:path ()
              else
                Server.respond_error ~status:`Not_found
                  ~body:("Instrumented script " ^ filename ^ " not found") ()
        | _ -> Server.respond_error ~status:`Not_found ~body:"" ()

let trace_collection_server_impl make_trace_sink =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let server = Server.make ~callback:(trace_collection_handler make_trace_sink) () in
  let%lwt conduit_ctx = Conduit_lwt_unix.init ~src:(Config.get_xhr_server_address()) () in
    Server.create
      ~ctx:(Cohttp_lwt_unix_net.init ~ctx:conduit_ctx ())
      ~mode:(`TCP (`Port (Config.get_xhr_server_port())))
      server

let trace_collection_server make_trace_sink =
  Lwt.finalize (fun () -> trace_collection_server_impl make_trace_sink)
    (fun () ->
       match !instrument_tmp_dir with
         | None -> Lwt.return ()
         | Some base ->
             Lwt.return (FileUtil.rm ~recurse:true ~force:FileUtil.Force [base]))

let one_shot_server trace_consumer =
  let result = Lwt_mvar.create_empty ()
  and waiting = ref true in
  let server =
    trace_collection_server
      (fun ~init_data ~id ->
         if !waiting then begin
           waiting := false;
           let (initials, stream, push) =
             TraceStream.parse_setup_packet init_data in
           let%lwt _ = Lwt_mvar.put result (trace_consumer initials stream) in
             Lwt.return_some push
         end else
           Lwt.return_none)
  in Lwt.ignore_result server; Lwt.bind (Lwt_mvar.take result) (fun x -> x)

let generic_server trace_consumer =
  trace_collection_server (fun ~init_data ~id ->
                             let (initials, stream, push) =
                               TraceStream.parse_setup_packet init_data
                             in (trace_consumer id initials stream: unit);
                                Lwt.return_some push)

