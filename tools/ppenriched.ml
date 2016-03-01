type mode =
    NoEnrichment
  | Arguments
  | ArgumentsAndClosures
  | NamesResolved
  | VersionedResolved
  | PointsToResolved

let filter = ref false
let delta = ref false

let calculate_filter_bound objects =
  let open Types in
  let seen = Hashtbl.create (BatDynArray.length objects) in
  let rec find_max max i =
    let max = Pervasives.max max i in
    let next = BatDynArray.get objects i in
      StringMap.fold (fun _ { value } (max: int) ->
                        if Hashtbl.mem seen value then
                          max
                        else begin
                          Hashtbl.add seen value ();
                          match try_get_object value with
                            | Some j -> find_max max j
                            | None -> max
                        end)
        next max
  in find_max 0 0 + 1

let delta_encode mkdelta init trace =
  let rec mkpairs_impl vold = function
    | [] -> []
    | (_, vnew) :: l -> mkdelta vold vnew :: mkpairs_impl vnew l
  in let mkpairs = function
    | [] -> [init]
    | (_, v0) :: l ->
        v0 :: mkpairs_impl v0 l
  in if !delta then
    List.map2 (fun (op, _) data -> (op, data))
      trace (mkpairs trace)
  else
    trace

let pp_enriched_trace_delta fmt mkdelta init =
  Fmt.using (delta_encode mkdelta init)
    (Fmt.vbox (Fmt.list ~sep:(Fmt.prefix Fmt.cut Fmt.cut)
                 (Fmt.pair ~sep:(Fmt.always " with@ ")
                    TraceTypes.pp_clean_operation fmt)))

let pp_enriched_trace_versions =
  let open LocalFacts in
  pp_enriched_trace_delta LocalFacts.pp_versions_resolved
    (fun { versions = old_versions; names = old_names }
           ({ versions; names } as facts) ->
       { facts with
             versions = Reference.ReferenceMap.merge
                          (fun _ vold vnew -> if vold = None then vnew else None)
                          old_versions versions;
             names = StringMap.merge
                       (fun _ vold vnew -> if vold = None then vnew else None)
                       old_names names })
    { last_arguments = None;
      closures = IntMap.empty;
      last_update = None;
      versions = Reference.ReferenceMap.empty;
      names = StringMap.empty }

let pp_enriched_trace_points_to =
  let open LocalFacts in
  pp_enriched_trace_delta LocalFacts.pp_local_facts
    (fun { versions = old_versions; names = old_names; points_to = old_points_to }
           ({ versions; names; points_to } as facts) ->
       { facts with
             versions = Reference.ReferenceMap.merge
                          (fun _ vold vnew -> if vold = None then vnew else None)
                          old_versions versions;
             names = StringMap.merge
                       (fun _ vold vnew -> if vold = None then vnew else None)
                       old_names names;
             points_to = Reference.VersionedReferenceMap.merge
                          (fun _ vold vnew -> if vold = None then vnew else None)
                          old_points_to points_to;
       })
    { last_arguments = None;
      closures = IntMap.empty;
      last_update = None;
      versions = Reference.ReferenceMap.empty;
      names = StringMap.empty;
      points_to = Reference.VersionedReferenceMap.empty
    }

let enrich_and_print mode
      (functions, objects, trace, globals, globals_are_properties) =
  let open Types in
  let open TraceTypes in
  let open LocalFacts in
  let initials = { functions; objects; globals; globals_are_properties } in
  let module Step1 = LocalFacts.CollectArguments(Streaming.ListTransformers) in
  let module Step2 = LocalFacts.CollectClosures(Streaming.ListTransformers) in
  let module Step3 = CalculateNameBindings.Make(Streaming.ListTransformers) in
  let module Step4 = CalculateVersions.Make(Streaming.ListTransformers) in
  let module Step5 = CalculatePointsTo.Make(Streaming.ListTransformers) in
    if !filter then begin
      LocalFacts.filter_bound := calculate_filter_bound objects
    end;
    match mode with
      | NoEnrichment ->
          trace |>
            pp_clean_trace Fmt.stdout
      | Arguments ->
          trace |>
            Step1.collect |>
            pp_enriched_trace (Fmt.option Fmt.int) Fmt.stdout
      | ArgumentsAndClosures ->
          trace |>
            Step1.collect |>
            Step2.collect |>
            pp_enriched_trace pp_arguments_and_closures Fmt.stdout
      | NamesResolved ->
          trace |>
            Step1.collect |>
            Step2.collect |>
            Step3.collect initials |>
            pp_enriched_trace pp_names_resolved Fmt.stdout
      | VersionedResolved ->
          trace |>
            Step1.collect |>
            Step2.collect |>
            Step3.collect initials |>
            Step4.collect initials |>
            pp_enriched_trace_versions Fmt.stdout
      | PointsToResolved ->
          trace |>
            Step1.collect |>
            Step2.collect |>
            Step3.collect initials |>
            Step4.collect initials |>
            Step5.collect initials |>
            pp_enriched_trace_points_to Fmt.stdout

let () =
  let files = ref []
  and mode = ref NoEnrichment
  and debug = ref false in
    Arg.parse
      [("-D", Arg.Set debug, "Debugging mode");
       ("-0", Arg.Unit (fun () -> mode := NoEnrichment), "Perform no enrichment");
       ("-1", Arg.Unit (fun () -> mode := Arguments), "Perform one step of enrichment");
       ("-2", Arg.Unit (fun () -> mode := ArgumentsAndClosures), "Perform two steps of enrichment");
       ("-3", Arg.Unit (fun () -> mode := NamesResolved), "Perform three steps of enrichment");
       ("-4", Arg.Unit (fun () -> mode := VersionedResolved), "Perform four steps of enrichment");
       ("-5", Arg.Unit (fun () -> mode := PointsToResolved), "Perform five steps of enrichment");
       ("-V", Arg.Unit Debug.enable_validate, "Enable validation");
       ("-f", Arg.Set filter, "Calculate filter bound");
       ("-d", Arg.Set delta, "Display deltas");
      ]
      (fun file -> files := !files @ [file])
      "ppcleantrace [-D] [-V] files";
    Log.default_setup !debug;
    List.iter
      (fun file -> file
         |> open_in
         |> Trace.parse_tracefile
         |> CleanTrace.clean_tracefile
         |> enrich_and_print !mode)
      !files
