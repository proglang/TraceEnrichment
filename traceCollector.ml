(** HTTP reply functions. *)
let reply_text ?(status=`OK) content body =
  let headers = Cohttp.Header.init_with "Content-Type" content in
    Cohttp_lwt_unix.Server.respond_string ~headers ~status ~body ()
let reply_plain_text ?(status=`OK) body = reply_text ~status "text/plain" body
let reply_json_text ?(status=`OK) body = reply_text ~status "application/json" body
let reply_html ?(status=`OK) body = reply_text ~status "text/html" body
let reply_javascript ?(status=`OK) body = reply_text ~status "application/javascript" body
let reply_error status body =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
    Cohttp_lwt_unix.Server.respond_error ~status ~body ~headers ()
let reply_file content filename =
  let headers = Cohttp.Header.init_with "Content-Type" content in
    Cohttp_lwt_unix.Server.respond_file ~headers ~fname:filename ()

let make_new_uuid () =
  Uuidm.v4_gen (Random.get_state ()) ()
    |> Uuidm.to_string

type handler =
    Uri.t -> string Lwt.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

module type STRATEGY = sig
  val make_trace_sink:
    init_data:string ->
    id:string ->
    finish:(unit -> unit) ->
    (string -> unit) Lwt.t
  val handlers_global: ((string * Cohttp.Code.meth) * handler) list
  val handlers_local: ((string * Cohttp.Code.meth) * (string -> handler)) list
end

module Server(S: STRATEGY) = struct
  open Cohttp.Code

  let sessions = Hashtbl.create 20
  let shutdown = ref false

  let handler_new uri body =
    let id = make_new_uuid () in
    let%lwt init_data = body in
    let%lwt sink =
      S.make_trace_sink ~init_data ~id ~finish:(fun () -> shutdown := true)
    in
      Hashtbl.add sessions id sink;
      reply_plain_text id

  let handler_instrument uri body =
    try 
      let uri = Uri.path uri in
      let basename =
        if uri = "" then None else Some (Filename.basename uri)
      in let%lwt base =
        JalangiInterface.instrument_for_browser ?basename
          ~providejs:(fun path ->
                        Lwt_io.with_file ~mode:Lwt_io.Output path
                          (fun channel -> Lwt.bind body (Lwt_io.write channel)))
      in reply_plain_text base
    with Exit -> reply_error `Not_found "No such handler"

  let handler_shutdown uri body =
    shutdown := true;
    reply_plain_text "Shuting down"

  let handler_facts id uri body =
    if Uri.path uri = "" then begin
      let%lwt body = body in
      Hashtbl.find sessions id body;
      reply_plain_text ~status:`Accepted "Accepted for processing"
    end else
      reply_error `Not_found "No such handler"

  let handler_index query =
    reply_plain_text "No implementation so far"

  let handle_session_management id meth =
    match meth with
      | `DELETE ->
          if Hashtbl.mem sessions id then begin
            let sink = Hashtbl.find sessions id in
              Hashtbl.remove sessions id;
              begin try sink "[['end']]" with _ -> () end;
              reply_plain_text "removed"
          end else
            reply_error `Not_found "No such resource"
      | _ ->
          reply_error `Method_not_allowed "Cannot access using this method"

  let handlers_global =
    let open Cohttp.Code in
    (("new", `POST), handler_new) ::
    (("instrument", `POST), handler_instrument) ::
    (("shutdown", `POST), handler_shutdown) ::
    S.handlers_global

  let handlers_local =
    (("facts", `POST), handler_facts) ::
    S.handlers_local

  let colon_re = Str.regexp ":"
  let html_filename_re = Str.regexp "^[-a-zA-Z0-9._]*\\.html$"
  let js_filename_re = Str.regexp "^[-a-zA-Z0-9._]*\\.js$"

  let multiplex conn req body =
    let open Cohttp.Request in
    let uri = uri req
    and body = Cohttp_lwt_body.to_string body
    and meth = meth req
    in let update_uri tail = Uri.with_path uri (BatString.concat "/" tail)
    in match Str.split colon_re (Uri.path uri) with
        | [] ->
            if meth = `GET then
              handler_index (Uri.query uri)
            else
              reply_error `Method_not_allowed ("Can't access / using this method")
        | [id] when Hashtbl.mem sessions id ->
            handle_session_management id meth
        | id::op::tail when Hashtbl.mem sessions id ->
            begin try
              List.assoc (op, meth) handlers_local id (update_uri tail) body
            with Not_found ->
              reply_error `Not_found ("No handler found")
            end
        | op::tail ->
            begin try
              List.assoc (op, meth) handlers_global (update_uri tail) body
            with Not_found ->
              let%lwt insdir = JalangiInterface.get_instrumented_dir () in
              let path = Filename.concat insdir op in
              if tail = [] && Sys.file_exists path then begin
                if Str.string_match html_filename_re op 0 then
                  reply_file "text/html" path
                else if Str.string_match js_filename_re op 0 then
                  reply_file "application/javascript" path
                else
                  reply_error `Forbidden "Cannot access this file"
              end else
                reply_error `Not_found "No handler found"
            end

  let server =
    let server = Cohttp_lwt_unix.Server.make ~callback:multiplex () in
    let%lwt ctx = Conduit_lwt_unix.init ~src:(Config.get_xhr_server_address ()) () in
      Cohttp_lwt_unix.Server.create
        ~ctx:(Cohttp_lwt_unix_net.init ~ctx ())
        ~mode:(`TCP (`Port (Config.get_xhr_server_port ())))
        server

end
