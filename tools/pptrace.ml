let files = ref [];;

Arg.parse [("-D", Arg.Unit Debug.enable, "Debugging mode")]
  (fun file -> files := !files @ [file])
  "ppcleantrace [-D] files";

List.iter (fun file -> file
             |> open_in
             |> Trace.parse_tracefile
             |> Format.printf "%a@." TraceTypes.pp_tracefile)
  !files
