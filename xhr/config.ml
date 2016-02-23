type 'a argument = 'a option ref
let node_path: string ref = ref "/usr/bin/node"
let jalangi2_path: string argument = ref None
let analysis_script_path: string argument = ref None
let xhr_server_address: string ref = ref "localhost"
let xhr_server_port: int ref = ref 8888
let keep_temporaries: bool ref = ref false
let setup_done = ref false

let set_string_arg arg = Arg.String (fun s -> arg := Some s)

let args =
  let open Arg in
    [ ("--jalangi", set_string_arg jalangi2_path, "Path to the Jalangi2 installation");
      ("--analysis", set_string_arg analysis_script_path, "Path to the analysis script");
      ("--xhr-bind", Set_string xhr_server_address, "Bind the XHR server to this address");
      ("--xhr-port", Set_int xhr_server_port, "Bind the XHR server to this port");
      ("--node", Set_string node_path, "Path to the node.js binary");
      ("--keep", Set keep_temporaries, "Keep temporary files and directories");
    ]

let find_node () =
  if Sys.command (!node_path ^ " -e 0") == 0 then
    ()
  else
    failwith "No working node installation found"

let (/:) = Filename.concat

let find_jalangi () =
  let good path = Sys.file_exists (path /: "src" /: "js" /: "commands" /: "instrument.js")
  in match !jalangi2_path with
    | Some path ->
        if good path then () else failwith "No jalangi2 installation found in given path!"
    | None ->
        if good "." then
          jalangi2_path := Some "."
        else
          let npm = Filename.dirname !node_path /: "npm" in
          let chan = Unix.open_process_in (npm ^ " explore jalangi2 -- pwd 2>/dev/null") in
            try
              let path = input_line chan in
                ignore (Unix.close_process_in chan);
                if good path then
                  jalangi2_path := Some path
                else
                  failwith "No jalangi2 installation found!"
            with _ ->
              ignore (Unix.close_process_in chan);
              failwith "No jalangi2 installation found!"

let rec find_first fgood = function
  | [] -> None
  | x::l -> if fgood x then Some x else find_first fgood l

let find_analysis_script () =
  let script_base = "generic-tracer.js" in
  let good path = Sys.file_exists (path /: script_base)
  in match !analysis_script_path with
    | Some path ->
        if good path then () else failwith "Analysis script not found in given path!"
    | None ->
        match find_first good [ "."; "tracer"; CompilationConfig.datadir /: "jstools" ] with
          | Some path -> analysis_script_path := Some path
          | None -> failwith "Analysis script not found!"

let setup () =
  if not !setup_done then begin
    find_jalangi();
    find_analysis_script();
    setup_done := true
  end


let get_node_path () = !node_path
let get_jalangi2_path () = setup (); BatOption.get !jalangi2_path
let get_analysis_script_path () = setup (); BatOption.get !analysis_script_path
let get_xhr_server_address () = !xhr_server_address
let get_xhr_server_port () = !xhr_server_port
let get_keep_temporaries () = !keep_temporaries
