open Lwt
open Cohttp
open Cohttp_lwt_unix

let pp_uri = Uri.pp_hum
let pp_method pp meth = Format.pp_print_string pp (Code.string_of_method meth)
let handler conn req body =
  let%lwt body_string = Cohttp_lwt_body.to_string body in
    Format.printf "Got request on %a using %a with body @[<hov 2>%s@]@."
      pp_uri (Request.uri req)
      pp_method (Request.meth req)
      body_string;
    if Uri.path (Request.uri req) = "/new" then
      Server.respond_string ~status:`OK ~body:"key" ()
    else
      Server.respond_string ~status:`Accepted ~body:"" ()

let test_server () =
  let server = Server.make ~callback:handler () in
    Server.create ~mode:(`TCP (`Port 8080)) server

let _ = Lwt_main.run (test_server ())
