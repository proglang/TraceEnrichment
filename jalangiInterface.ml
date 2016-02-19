let (/:) = Filename.concat

let instrument_script_path =
  List.fold_right (/:) [ "src"; "js"; "commands" ] "instrument.js"

exception InstrumentationError

let template_cache = CamlTemplate.Cache.create ()

let instrument_make_html basename =
  let model = Hashtbl.create 1
  and buffer = Buffer.create 512
  and tmpl = CamlTemplate.Cache.get_template
               template_cache
               (Config.get_analysis_html_path ())
  in
    Hashtbl.add model "basename" (CamlTemplate.Model.Tstr (Filename.basename basename));
    CamlTemplate.merge tmpl model buffer;
    Buffer.contents buffer

let unlink_files files =
  Lwt.join (List.map (fun file ->
                        Lwt.catch
                          (fun () -> Lwt_unix.unlink file)
                          (fun _ -> Lwt.return_unit)) files)

let jalangi2_instrument strategy filenames outdir =
  let open Lwt in
  let args =
    [ "node";
      Config.get_jalangi2_path() /: instrument_script_path;
      "--inlineIID";
      "--inlineSource";
      "--inlineJalangi";
      "-i";
      "--analysis"; Config.get_analysis_script_path ();
      "--initParam"; "host:" ^ (Config.get_xhr_server_address ());
      "--initParam"; "port:" ^ (string_of_int (Config.get_xhr_server_port ()));
      "--initParam"; "strategy:" ^ strategy;
      "--outputDir"; outdir;
    ] @ filenames
  in let argarray = Array.of_list args in
  match Lwt_unix.fork () with
  | 0 ->
      Unix.close Unix.stdout;
      Unix.close Unix.stderr;
      Unix.execv (Config.get_node_path ()) argarray
  | pid ->
    match%lwt Lwt_unix.waitpid [] pid with
      | (_, Unix.WEXITED 0) ->
          (* Clean up. *)
          if Config.get_keep_temporaries () then
            Lwt.return_unit
          else
            Lwt_list.iter_p
              (fun filename ->
                 let base = Filename.basename filename in
                 if Filename.check_suffix base ".js" then begin
                   let base = outdir /: Filename.chop_suffix base ".js" in
                     unlink_files [ base ^ "_jalangi_.json"; base ^ "_orig_.js" ]
                 end else
                   Lwt.return_unit)
              filenames
      | _ -> raise InstrumentationError

let instrument_tmp_dir = ref None

let get_instrument_tmp_dir () =
  match !instrument_tmp_dir with
  | Some dir -> Lwt.return (dir, dir /: "instrumented")
  | None ->
    let%lwt path = Files.temp_dir_lwt "jstools" "" in
    let%lwt _ = Lwt_unix.mkdir (path /: "instrumented") 0o700 in
    instrument_tmp_dir := Some path;
    Lwt.return (path, path /: "instrumented")

let get_instrumented_dir () = Lwt.map snd (get_instrument_tmp_dir ())
let clean_up () =
  match !instrument_tmp_dir with
    | Some base ->
        if not (Config.get_keep_temporaries ()) then
          ignore (Sys.command ("rm -rf " ^ base))
    | None -> ()

let instrument_for_browser ?basename ~providejs =
  let%lwt (tmpdir, insdir) = get_instrument_tmp_dir () in
  let basename = match basename with
    | Some basename -> tmpdir /: Filename.basename basename
    | None -> Filename.temp_file ~temp_dir:tmpdir "gen" ""
  in let jsfile = basename ^ ".js" and htmlfile = basename ^ ".html" in
  let%lwt () = providejs jsfile in
  let driver = instrument_make_html basename in
  let%lwt () = Lwt_io.with_file Lwt_io.Output htmlfile (fun c -> Lwt_io.write c driver) in
  let%lwt () = jalangi2_instrument "xhr" [ jsfile; htmlfile ] insdir in
  let%lwt () =
    if Config.get_keep_temporaries () then
      Lwt.return_unit
    else
      unlink_files [ jsfile; htmlfile; basename ]
  in Lwt.return (insdir /: Filename.basename basename)
   
