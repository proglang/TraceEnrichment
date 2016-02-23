let () =
  let files = ref []
  and debug = ref false in
    Arg.parse
      [("-D", Arg.Set debug, "Debugging mode");
       ("-V", Arg.Unit Debug.enable_validate, "Enable validation");
      ]
      (fun file -> files := !files @ [file])
      "ppcleantrace [-D] [-V] files";
    Log.default_setup !debug;
    List.iter
      (fun file -> file
         |> open_in
         |> Trace.parse_tracefile
         |> CleanTrace.clean_tracefile
         |> Printexc.print (Format.printf "%a@." TraceTypes.pp_clean_tracefile))
      !files