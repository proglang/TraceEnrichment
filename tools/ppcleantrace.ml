let () =
  let level = ref CleanTrace.SynthesizeEvents in
  let files = ref []
  and debug = ref false in
    Arg.parse
      [("-D", Arg.Set debug, "Debugging mode");
       ("-V", Arg.Unit Debug.enable_validate, "Enable validation");
       ("-1", Arg.Unit (fun () -> level := CleanTrace.JustClean), "Only clean, no normalization");
       ("-2", Arg.Unit (fun () -> level := CleanTrace.Normalizations), "No call or getter/setter normalization");
       ("-3", Arg.Unit (fun () -> level := CleanTrace.SynthesizeGettersAndSetters), "No call normalization");
       ("-4", Arg.Unit (fun () -> level := CleanTrace.SynthesizeEvents), "Full normalization");
      ]
      (fun file -> files := !files @ [file])
      "ppcleantrace [-D] [-V] files";
    Log.default_setup !debug;
    List.iter
      (fun file -> file
         |> Trace.read_tracefile
         |> CleanTrace.clean_tracefile ~up_to:!level
         |> Printexc.print (Format.printf "%a@." TraceTypes.pp_clean_tracefile))
      !files
