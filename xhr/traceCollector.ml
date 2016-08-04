(** The core of the XHR server. *)

(** {1 HTTP reply functions.} *)

(** Reply with some kind of text. *)
let reply_text ?(status=`OK) content body =
  let headers = Cohttp.Header.init_with "Content-Type" content in
    Cohttp_lwt_unix.Server.respond_string ~headers ~status ~body ()
(** Reply with plain text. *)
let reply_plain_text ?(status=`OK) body = reply_text ~status "text/plain" body
(** Reply with JSON. *)
let reply_json_text ?(status=`OK) body = reply_text ~status "application/json" body
(** Reply with HTML. *)
let reply_html ?(status=`OK) body = reply_text ~status "text/html" body
(** Reply with JavaScript. *)
let reply_javascript ?(status=`OK) body = reply_text ~status "application/javascript" body
(** Reply with binary data. *)
let reply_binary ?(status=`OK) body = reply_text ~status "application/octet-stream" body
(** Reply with an error. *)
let reply_error status body =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
    Cohttp_lwt_unix.Server.respond_error ~status ~body ~headers ()
(** Reply with a file. *)
let reply_file content filename =
  let headers = Cohttp.Header.init_with "Content-Type" content in
    Cohttp_lwt_unix.Server.respond_file ~headers ~fname:filename ()
(** Reply with a redirect. *)
let reply_redirect uri =
  Cohttp_lwt_unix.Server.respond_redirect ~uri ()

(**/**)
let make_new_uuid () =
  Uuidm.v4_gen (Random.get_state ()) ()
    |> Uuidm.to_string

let (/:) = Filename.concat

let instrument uri providejs =
  try 
    let uri = Uri.path uri in
    let basename =
      if uri = "" then None else Some (Filename.basename uri)
    in
      Log.info (fun m -> m "Instrumenting JavaScript%a"
                           (Fmt.option (Fmt.prefix (Fmt.const Fmt.string " with basename ") Fmt.string))
                           basename);
      let%lwt base =
        JalangiInterface.instrument_for_browser ?basename ~providejs
      in
        Log.info (fun m -> m "Performed instrumentation, resulting in %s" base);
        reply_plain_text (Filename.basename base)
  with
      e -> Log.warn (fun m -> m "Exception in instrumentation: %s" (Printexc.to_string e));
           reply_error `Internal_server_error (Printexc.to_string e)
(**/**)

(** {1 Server implementation} *)
(** Type of URL handlers. *)
type handler =
    Uri.t -> string Lwt.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

(** A module type for strategies implementating XHR servers. *)
module type STRATEGY = sig
  (** Generate a trace sink, where event packets pertaining to a specific trace 
      can be sent and handled.

      Call as [make_trace_sink init_data id finish]. [init] containes the initial
      data packet (as a string), [id] a unique ID for the trace being handled,
      and [finish] a function that is called when the input trace has finished.
      It returns a function that can be fed trace event packets (as strings).
  *)
  val make_trace_sink:
    init_data:string ->
    id:string ->
    finish:(unit -> unit) ->
    (string -> unit) Lwt.t

    (** Global handlers to provide by the server. Each entry is of the form
        [((path, method), (desc, handler))], [path] and [method] are used to
        construct the URL under which the handler can be reached,
        [desc] is a human-readable description of the method, and
        [handler] is a function to handle the request, taking the URI of the
        request and the request body as arguments and returning a CohTTP response. *)
  val handlers_global: ((string * Cohttp.Code.meth) * (string * handler)) list
    (** Local handlers to provide by the server. Each entry is of the form
        [((path, method), (desc, handler))], [path] and [method] are used to
        construct the URL under which the handler can be reached,
        [desc] is a human-readable description of the method, and
        [handler] is a function to handle the request, taking the session id,
        the URI of the request and the request body as arguments and returning
        a CohTTP response. *)
  val handlers_local: ((string * Cohttp.Code.meth) * (string * (string -> handler))) list
end

(** Functor for creating servers. *)
module Server(S: STRATEGY): sig
  (** Server implementation. *)
  val server: unit -> unit Lwt.t
end = struct
  open Cohttp.Code

  (**/**)
  let sessions = Hashtbl.create 20

  let (stop, wakener) = Lwt.wait ()
  let shutdown: unit -> unit = Lwt.wakeup wakener

  let operations_view ops =
    let open Cohttp.Code in
    let open Jg_types in
    let ops =
      List.filter (function ((_, (`GET | `POST)), _) -> true | _ -> false) ops
    in let ops_view =
      List.map
        (fun ((op, meth), (name, _)) ->
           Tobj [("path", Tstr op);
                 ("post", Tbool (meth = `POST));
                 ("name", Tstr name)])
        ops
    in Tlist ops_view

  let global_operations_view = operations_view S.handlers_global
  let local_operations_view = operations_view S.handlers_local

  let handler_new uri body =
    let id = make_new_uuid () in
    let%lwt init_data = body in
    Log.info (fun m -> m "Adding new handler for %s based on %s" id init_data);
    let%lwt sink =
      S.make_trace_sink ~init_data ~id ~finish:shutdown
    in
      Hashtbl.add sessions id sink;
      reply_plain_text id

  let handler_instrument uri body =
    instrument uri
      (fun path ->
         Lwt_io.with_file ~mode:Lwt_io.Output path
           (fun channel -> Lwt.bind body (Lwt_io.write channel)))

  let handler_shutdown uri body =
    Log.info (fun m -> m "Shutting down server");
    shutdown ();
    reply_plain_text "Shuting down"

  let handler_facts id uri body =
    if Uri.path uri = "" then begin
      let%lwt body = body in
      Hashtbl.find sessions id body;
      reply_plain_text ~status:`Accepted "Accepted for processing"
    end else begin
      Log.info (fun m -> m "Fact handling with extra arguments");
      reply_error `Not_found "No such handler"
    end

  let handler_index query =
    let open Jg_types in
    (** Create a nice list of available operations and files. *)
    let num_files = 256 in
    let%lwt dir = JalangiInterface.get_instrumented_dir () in
    let%lwt dirhandle = Lwt_unix.opendir dir in
    let rec collect_files seen =
      let%lwt more_files = Lwt_unix.readdir_n dirhandle num_files in
      let seen = Array.to_list more_files @ seen in
        if Array.length more_files < num_files then
          let%lwt () = Lwt_unix.closedir dirhandle in
          Lwt.return (List.sort String.compare seen)
        else
          collect_files seen
    in let%lwt files = collect_files [] in
    let files = Tlist (List.map (fun n -> Tstr n) files)
    and sessions_view = ref [] in
      Hashtbl.iter (fun key _ -> sessions_view := Tstr key :: !sessions_view) sessions;
      let page = Jg_template.from_string
           ~models:[ ("global_operations", global_operations_view);
                     ("local_operations", local_operations_view);
                     ("instrumented_failes", files);
                     ("session", Tlist !sessions_view)]
           PageText.trace_collector_index
      in reply_html page

  let handle_session_management uri id meth =
    match meth with
      | `DELETE ->
          Log.info (fun m -> m "Deleting session %s" id);
          if Hashtbl.mem sessions id then begin
            let sink = Hashtbl.find sessions id in
              Hashtbl.remove sessions id;
              begin try sink "[[\"end\"]]" with _ -> () end;
              reply_plain_text "removed"
          end else
            reply_error `Not_found "No such resource"
      | `GET ->
          Log.info (fun m -> m "Menu of local operations for %s" id);
          (* Special-case if there is only one operation, using GET *)
          begin match S.handlers_local with
            | [ ((op, `GET), _) ] ->
                reply_redirect (Uri.with_path uri (id ^ "/" ^ op))
            | _ ->
                let page = Jg_template.from_string
                  ~models:[ ("local_operations", local_operations_view);
                            ("session", Jg_types.Tstr id) ]
                  PageText.operation_menu
                in reply_html page
          end
      | _ ->
          Log.info (fun m -> m "Session management with bad method");
          reply_error `Method_not_allowed "Cannot access using this method"

  let handlers_global =
    let open Cohttp.Code in
    (("new", `POST), ("new trace session", handler_new)) ::
    (("instrument", `POST), ("instrument JavaScript source code", handler_instrument)) ::
    (("shutdown", `POST), ("shut down server", handler_shutdown)) ::
    S.handlers_global

  let handlers_local =
    (("facts", `POST), ("add facts in a trace session", handler_facts)) ::
    S.handlers_local

  let slash_re = Str.regexp "/"
  let html_filename_re = Str.regexp "^[-a-zA-Z0-9._]*\\.html$"
  let js_filename_re = Str.regexp "^[-a-zA-Z0-9._]*\\.js$"

  let multiplex conn req body =
    try
      let open Cohttp.Request in
      let uri = uri req
      and body = Cohttp_lwt_body.to_string body
      and meth = meth req
      in Log.info (fun m -> m "Received request for %s using %s"
                              (Uri.to_string uri) (Cohttp.Code.string_of_method meth));
         let update_uri tail = Uri.with_path uri (BatString.concat "/" tail)
         in
           match Str.split slash_re (BatString.strip ~chars:"/" (Uri.path uri)) with
             | [] ->
                 if meth = `GET then
                   handler_index (Uri.query uri)
                 else begin
                   Log.info (fun m -> m "Accessing index with bad method");
                   reply_error `Method_not_allowed ("Can't access / using this method")
                 end
             | [id] when Hashtbl.mem sessions id ->
                 handle_session_management uri id meth
             | id::op::tail when Hashtbl.mem sessions id ->
                 begin try
                   let (_, handler) = List.assoc (op, meth) handlers_local in
                     handler id (update_uri tail) body
                 with
                     Not_found ->
                       Log.info (fun m -> m "No handler found for %s, session %s" op id);
                       reply_error `Not_found ("No handler found")
                   | e ->
                       Log.err (fun m -> m "Got exception %s" (Printexc.to_string e));
                       raise e
                 end
             | op::tail ->
                 begin try
                   let (_, handler) = List.assoc (op, meth) handlers_global in
                     handler (update_uri tail) body
                 with Not_found ->
                   let%lwt insdir = JalangiInterface.get_instrumented_dir () in
                   let path = Filename.concat insdir op in
                     if tail = [] && Sys.file_exists path then begin
                       if Str.string_match html_filename_re op 0 then begin
                         Log.info (fun m -> m "Serving HTML file");
                         reply_file "text/html" path
                       end else if Str.string_match js_filename_re op 0 then begin
                         Log.info (fun m -> m "Serving JS file");
                         reply_file "application/javascript" path
                       end else begin
                         Log.info (fun m -> m "Trying to serve inaccesible file");
                         reply_error `Forbidden "Cannot access this file"
                       end
                     end else begin
                       Log.info (fun m -> m "No handler available");
                       reply_error `Not_found "No handler found"
                     end
                   | e ->
                       Log.err (fun m -> m "Got exception %s" (Printexc.to_string e));
                       raise e
                 end
    with 
      | e -> Log.err (fun m -> m "Got exception: %s" (Printexc.to_string e));
             raise e
  (**/**)

  (** Server implementation. *)
  let server () =
    let open Lwt in
      Log.debug (fun m -> m "Starting JSCollector server");
      let server = Cohttp_lwt_unix.Server.make ~callback:multiplex () in
        Log.info (fun m -> m "Bind address: %s" (Config.get_xhr_server_address ()));
        Log.info (fun m -> m "Bind port: %d" (Config.get_xhr_server_port ()));
      let%lwt ctx = Conduit_lwt_unix.init ~src:(Config.get_xhr_server_address ()) () in
      let%lwt () =
        Cohttp_lwt_unix.Server.create
          ~stop
          ~ctx:(Cohttp_lwt_unix_net.init ~ctx ())
          ~mode:(`TCP (`Port (Config.get_xhr_server_port ())))
          server
      in Log.debug (fun m -> m "Stopped JSCollector server");
         JalangiInterface.clean_up();
         Lwt.return_unit

end
