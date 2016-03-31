let () =
  let files = ref []
  and debug = ref false in
    Arg.parse [("-D", Arg.Set debug, "Debugging mode")]
      (fun file -> files := !files @ [file])
      "ppcleantrace [-D] files";
    Log.default_setup !debug;
    List.iter (fun file -> file
                 |> Trace.read_tracefile
                 |> Format.printf "%a@." TraceTypes.pp_tracefile)
      !files
