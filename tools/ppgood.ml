open TraceTypes

let nice_pp pp { trace } =
  List.iter (fun (op, { sid; tsid }) -> Format.fprintf pp "%0.2d-%0.2d %a@." sid tsid pp_rich_operation op) trace

let () =
  let files = ref [] in
    Arg.parse []
      (fun file -> files := !files @ [file])
      "ppcleantrace files";
    List.iter (fun file -> file
                 |> Trace.read_tracefile
                 |> RichTrace.tracefile_to_rich_tracefile
                 |> Fmt.vbox nice_pp Format.std_formatter)
      !files
