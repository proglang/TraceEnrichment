let () =
  let files = ref []
  and debug = ref false in
    Arg.parse
      [("-D", Arg.Set debug, "Debugging mode");
       ("-V", Arg.Unit Debug.enable_validate, "Enable validation");
       ("-f", Arg.Unit TraceTypes.enable_dump_facts, "Dump local facts")
      ]
      (fun file -> files := !files @ [file])
      "ppcleantrace [-D] [-V] [-f] files";
    Log.default_setup !debug;
    List.iter (fun file -> file
                 |> open_in
                 |> Trace.parse_tracefile
                 |> RichTrace.tracefile_to_rich_tracefile
                 |> Printexc.print (Format.printf "%a@." TraceTypes.pp_rich_tracefile))
      !files
