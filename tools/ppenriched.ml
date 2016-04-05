type mode =
    NoEnrichment
  | Arguments
  | ArgumentsAndClosures
  | NamesResolved
  | VersionedResolved
  | PointsToResolved

let filter = ref false
let delta = ref false
let debug = ref false
let output = ref true

let maybe fmt arg =
  if !output then fmt Fmt.stdout arg

let time name fn x =
  Log.info (fun m -> m "Starting %s" name);
  let start = Sys.time () in
  let result = fn x in
  let stop = Sys.time () in
  Log.info (fun m -> m "Finished %s, took ~%f seconds" name (stop-.start));
  result

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

let rec list_delta init diff = function
  | [] -> []
  | value :: list ->
      diff init value :: list_delta value diff list

let delta_encode init diff list =
  let (ops, data) = List.split list in
    List.combine ops (list_delta init diff data)

let pp_enriched_trace init diff fmt fmtdiff pp list =
  let double_sep = Fmt.cut
  and with_sep = Fmt.always " with@ " in
  let list_fmt fmt =
    Fmt.vbox (Fmt.list ~sep:double_sep
                 (Fmt.vbox ~indent:2 (Fmt.pair ~sep:with_sep
                    TraceTypes.pp_clean_operation
                    fmt))) in
  if !delta then
    Fmt.using (delta_encode init diff) (list_fmt fmtdiff) pp list
  else
    list_fmt fmt pp list

type versions_resolved_delta = {
  last_arguments : int option;
  closures : Reference.reference StringMap.t ExtMap.diff IntMap.t;
  last_update : Reference.versioned_reference option;
  versions : int ExtMap.diff Reference.ReferenceMap.t;
  names : Reference.reference ExtMap.diff StringMap.t;
  fresh_versioned_references: Reference.versioned_reference list
}
let filter_versions m =
  Reference.ReferenceMap.filter (fun key _ -> match key with
                                   | Reference.Field (Types.Object 0, _) -> true
                                   | Reference.Field (obj, _) -> Types.get_object_id obj >= !LocalFacts.filter_bound
                                   | _ -> true) m
let filter_points_to m =
  Reference.VersionedReferenceMap.filter (fun key _ -> match key with
                                   | (Reference.Field (Types.Object 0, _), _) -> true
                                   | (Reference.Field (obj, _), _) -> Types.get_object_id obj >= !LocalFacts.filter_bound
                                   | _ -> true) m

module FmtDiff(M: ExtMap.S) = struct
  let fmt pp_entry key pp map =
    if not (M.is_empty map) then
      Format.fprintf pp "@[<v 2>%s@ %a@]@ "
        key
        (M.pp ~pair_sep:(Fmt.always ": ")
           ~entry_sep:Fmt.cut (ExtMap.pp_diff pp_entry)) map
end
let fmt_versions pp versions =
  let module F = FmtDiff(Reference.ReferenceMap) in
    F.fmt Fmt.int "versions" pp (filter_versions versions)
let fmt_names =
  let module F = FmtDiff(StringMap) in
    F.fmt Reference.pp_reference "names"
let fmt_closures =
  let module F = FmtDiff(IntMap) in
    F.fmt (Fmt.braces (StringMap.pp Reference.pp_reference)) "closures"
let fmt_points_to pp pointsto =
  Reference.pp_versioned_reference_map
    (ExtMap.pp_diff Types.pp_jsval)
    pp (filter_points_to pointsto)
let fmt_option fmt key pp = function
  | Some x -> Format.fprintf pp "%s: %a@ " key fmt x
  | None -> ()
  

let pp_versions_resolved_delta pp 
      { last_arguments; closures; last_update; versions; names } =
  Format.pp_open_vbox pp 0;
  fmt_option Fmt.int "last_argument" pp last_arguments;
  fmt_option Reference.pp_versioned_reference "last_update" pp last_update;
  fmt_versions pp versions;
  fmt_names pp names;
  fmt_closures pp closures;
  Format.pp_close_box pp ()

let pp_enriched_trace_versions =
  let open LocalFacts in
  pp_enriched_trace 
    { last_arguments = None;
      closures = IntMap.empty;
      last_update = None;
      versions = Reference.ReferenceMap.empty;
      names = StringMap.empty;
      fresh_versioned_references = []
    }
    (fun { versions = old_versions; names = old_names; closures = old_closures }
           ({ versions; names; closures; last_update; last_arguments; fresh_versioned_references }) ->
       ({ last_update; last_arguments; fresh_versioned_references;
          closures = IntMap.delta (StringMap.equal (Reference.equal_reference)) old_closures closures;
          versions = Reference.ReferenceMap.delta (=) old_versions versions;
          names = StringMap.delta (Reference.equal_reference) old_names names }: versions_resolved_delta))
    pp_versions_resolved pp_versions_resolved_delta
    
type local_facts_delta = {
  last_arguments : int option;
  closures : Reference.reference StringMap.t ExtMap.diff IntMap.t;
  last_update : Reference.versioned_reference option;
  versions : int ExtMap.diff Reference.ReferenceMap.t;
  names : Reference.reference ExtMap.diff StringMap.t;
  points_to: Types.jsval ExtMap.diff Reference.VersionedReferenceMap.t
}
let pp_local_facts_delta pp 
      { last_arguments; closures; last_update; versions; names; points_to } =
  Format.pp_open_vbox pp 0;
  fmt_option Fmt.int "last_argument" pp last_arguments;
  fmt_option Reference.pp_versioned_reference "last_update" pp last_update;
  fmt_versions pp versions;
  fmt_names pp names;
  fmt_closures pp closures;
  fmt_points_to pp points_to;
  Format.pp_close_box pp ()

let pt_delta pt_old pt_new =
  let module M = Reference.VersionedReferenceMap in
    M.merge (fun _ -> function
               | `Both (xold, xnew) ->
                   if xold = xnew then
                     None
                   else
                     Some (ExtMap.Change (xold, xnew))
               | `Left xold -> Some (ExtMap.Remove xold)
               | `Right xnew -> Some (ExtMap.Add xnew))
      pt_old pt_new

let pp_enriched_trace_points_to =
  let open LocalFacts in
  pp_enriched_trace 
    { last_arguments = None;
      closures = IntMap.empty;
      last_update = None;
      versions = Reference.ReferenceMap.empty;
      names = StringMap.empty;
      points_to = Reference.VersionedReferenceMap.empty ()
    }
    (fun { versions = old_versions; names = old_names; closures = old_closures;
           points_to = old_points_to }
           ({ versions; names; closures; last_update; last_arguments;
              points_to }) ->
       ({ last_update; last_arguments;
          closures = IntMap.delta (StringMap.equal (Reference.equal_reference))
                       old_closures closures;
          versions = Reference.ReferenceMap.delta (=) old_versions versions;
          names = StringMap.delta Reference.equal_reference old_names names;
          points_to = pt_delta old_points_to points_to
       }: local_facts_delta))
    pp_local_facts pp_local_facts_delta

let enrich_and_print mode
      (functions, objects, trace, globals, globals_are_properties) =
  let open Types in
  let open TraceTypes in
  let open LocalFacts in
  let initials = { functions; objects; globals; globals_are_properties;
                   function_call = OUndefined;
                   function_apply = OUndefined;
                   function_eval = OUndefined;
                   function_constructor = OUndefined;
  } in
    lookup_functions initials;
  let module Step1 = LocalFacts.CollectArguments(Streaming.ListTransformers) in
  let module Step2 = LocalFacts.CollectClosures(Streaming.ListTransformers) in
  let module Step3 = CalculateNameBindings.Make(Streaming.ListTransformers) in
  let module Step4 = CalculateVersions.Make(Streaming.ListTransformers) in
  let module Step5 = CalculatePointsTo.Make(Streaming.ListTransformers) in
  let step1 = time "collecting arguments" Step1.collect
  and step2 = time "collecting closures" Step2.collect
  and step3 = time "calculating name bindings" (Step3.collect initials)
  and step4 = time "calculating versions" (Step4.collect initials)
  and step5 = time "calculating points-to" (Step5.collect initials) in
    if !filter then begin
      LocalFacts.filter_bound := calculate_filter_bound objects
    end;
    match mode with
      | NoEnrichment ->
          trace |>
            maybe pp_clean_trace
      | Arguments ->
          trace |>
            step1 |>
            maybe (pp_enriched_trace (Fmt.option Fmt.int))
      | ArgumentsAndClosures ->
          trace |>
            step1 |>
            step2 |>
            maybe (pp_enriched_trace pp_arguments_and_closures)
      | NamesResolved ->
          trace |>
            step1 |>
            step2 |>
            step3 |>
            maybe (pp_enriched_trace pp_names_resolved)
      | VersionedResolved ->
          trace |>
            step1 |>
            step2 |>
            step3 |>
            step4 |>
            maybe pp_enriched_trace_versions
      | PointsToResolved ->
          trace |>
            step1 |>
            step2 |>
            step3 |>
            step4 |>
            step5 |>
            maybe pp_enriched_trace_points_to

let () =
  let files = ref []
  and mode = ref NoEnrichment in
    Arg.parse
      [("-D", Arg.Set debug, "Debugging mode");
       ("-0", Arg.Unit (fun () -> mode := NoEnrichment), "Perform no enrichment");
       ("-1", Arg.Unit (fun () -> mode := Arguments), "Perform one step of enrichment");
       ("-2", Arg.Unit (fun () -> mode := ArgumentsAndClosures), "Perform two steps of enrichment");
       ("-3", Arg.Unit (fun () -> mode := NamesResolved), "Perform three steps of enrichment");
       ("-4", Arg.Unit (fun () -> mode := VersionedResolved), "Perform four steps of enrichment");
       ("-5", Arg.Unit (fun () -> mode := PointsToResolved), "Perform five steps of enrichment");
       ("-V", Arg.Unit Debug.enable_validate, "Enable validation");
       ("-d", Arg.Set delta, "Display deltas");
       ("-f", Arg.Set filter, "Calculate filter bound");
       ("-O", Arg.Clear output, "Skip output")
      ]
      (fun file -> files := !files @ [file])
      "ppcleantrace [-D] [-V] files";
    Log.default_setup !debug;
    if not !debug then begin
      Logs.set_level ~all:true (Some Logs.Info)
    end;
    List.iter
      (fun file -> file
         |> time "parsing trace file" Trace.read_tracefile
         |> time "generating clean trace" CleanTrace.clean_tracefile
         |> enrich_and_print !mode)
      !files
