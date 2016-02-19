let files = ref [];;

Arg.parse
  [("-D", Arg.Unit Debug.enable, "Debugging mode");
   ("-V", Arg.Unit Debug.enable_validate, "Enable validation");
  ]
  (fun file -> files := !files @ [file])
  "ppcleantrace [-D] [-V] files";

List.iter
  (fun file -> file
     |> open_in
     |> Trace.parse_tracefile
     |> CleanTrace.clean_tracefile
     |> Printexc.print (Format.printf "%a@." TraceTypes.pp_clean_tracefile))
  !files
