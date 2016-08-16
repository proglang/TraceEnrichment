open TypesJS
open TraceTypes
open LocalFacts
type versioned_reference = Reference.versioned_reference

let enrich_step globals_are_properties (op, facts) =
  let mkfieldref base offset =
    try
      Reference.reference_of_field base offset |> LocalFacts.make_versioned facts
    with Not_found ->
      Logs.err (fun m -> m "mkfieldref failed for %a:%s" pp_jsval base offset);
      raise Not_found
  and mkvarref name =
    try 
      Reference.reference_of_name globals_are_properties facts.names name
        |> LocalFacts.make_versioned facts
    with Not_found ->
      Logs.err (fun m -> m "mkvarref failed for %s" name);
      raise Not_found
  and mknamebindref name =
    try
      Reference.reference_of_name globals_are_properties facts.names name
        |> LocalFacts.make_versioned facts
    with Not_found ->
      Logs.err (fun m -> m "mknamebindref failed for %s" name);
      raise Not_found
  in let res = match op with
    | CFunPre { f; base; args; call_type } ->
      [RFunPre { f; base; args; call_type } ]
    | CFunPost { f; base; args; result; call_type } -> [RFunPost { f; base; args; result; call_type }]
    | CLiteral { value; hasGetterSetter } -> [RLiteral { value; hasGetterSetter }]
    | CForIn value -> [RForIn value]
    | CDeclare { name; value; declaration_type = ArgumentBinding idx } ->
      begin match StringMap.find name facts.names with
        | Reference.Field _ as ref ->
            let ref = begin
              try LocalFacts.make_versioned facts ref
              with Not_found ->
                Logs.err
                  (fun m ->
                     m "Finding versions for argument binding %d called %s failed; it maps to %a"
                       idx name Reference.pp_reference ref);
                raise Not_found
            end in
              [RAlias { name; ref; source = Argument idx };
               RWrite { ref; orig_ref = fst ref; oldref = ref;
                        value; success = true; isComputed = false } ]
        | Reference.Variable _ ->
            let ref = mknamebindref name
            in
              [RLocal { name; ref };
               RWrite { ref; orig_ref = fst ref; oldref = ref;
                        value = OUndefined; success = true; isComputed = false } ]
      end
    | CDeclare { name; value; declaration_type = CatchParam } ->
      let ref = mknamebindref name in
      [RCatch { name; ref };
       RWrite { ref; orig_ref = fst ref; oldref = ref; value;
                success = true; isComputed = false } ]
    | CDeclare { name; value } ->
      let ref = mknamebindref name in
      [RLocal { name; ref };
       RWrite { ref; orig_ref = fst ref; oldref = ref; value;
                success = true; isComputed = false } ]
    | CGetFieldPre _ -> Log.debug (fun m -> m "Unexpected get_field_pre"); []
    | CPutFieldPre _ -> Log.debug (fun m -> m "Unexpected put_field_pre"); []
    | CGetField { actual_base; base; offset; value; isComputed } ->
      [RRead { ref = mkfieldref actual_base offset;
               orig_ref = Reference.reference_of_field base offset;
               value; isComputed }]
    | CPutField { actual_base; base; offset; value; isComputed } ->
      (* FIXME success handling *)
      [RWrite {
          ref = mkfieldref actual_base offset;
          orig_ref = Reference.reference_of_field base offset;
          oldref = BatOption.get facts.last_update;
          value; success = true; isComputed
        }]
    | CRead { name; value } ->
      [RRead { ref = mkvarref name;
               orig_ref = Reference.reference_of_name globals_are_properties
                            facts.names name;
               value; isComputed = false }]
    | CWrite { name; lhs; value } ->
      (* FIXME success handling *)
      [RWrite {
          ref = mkvarref name;
          orig_ref = Reference.reference_of_name globals_are_properties
                       facts.names name;
          oldref = BatOption.get facts.last_update;
          value;
          success = true;
          isComputed = false
        }]
    | CReturn value -> [RReturn value]
    | CThrow value -> [RThrow value]
    | CWith value -> [RWith value]
    | CFunEnter { f; this; args } -> [RFunEnter { f; this; args }]
    | CFunExit { ret; exc } -> [RFunExit { ret; exc }]
    | CScriptEnter -> [RScriptEnter]
    | CScriptExit -> [RScriptExit]
    | CScriptExc exc -> [RScriptExc exc]
    | CBinary { op; left; right; result } -> [RBinary { op; left; right; result }]
    | CUnary { op; arg; result } -> [RUnary { op; arg; result }]
    | CEndExpression -> [REndExpression]
    | CConditional (iid, value) -> [RConditional (iid, value)] in
  List.map (fun op -> (op, facts)) res

module ToRich(S: Streaming.Transformers) = struct
  let enriched_trace_to_rich_trace globals_are_properties (trace: (clean_operation * local_facts) S.sequence) =
    trace
      |> S.map (fun (op, ({ last_update; versions; points_to; names }: local_facts)) ->
                   (op, {last_update; versions; points_to; names }))
      |> S.map_list (enrich_step globals_are_properties)
  module C = CleanTrace.CleanGeneric(S)
  module E = EnrichTrace.Make(S)
  let trace_to_rich_trace initials trace =
    trace
      |> C.calculate_clean_trace initials
      |> E.collect initials
      |> enriched_trace_to_rich_trace initials.globals_are_properties
end;;

module ListToRich = ToRich(Streaming.ListTransformers)
module StreamToRich = ToRich(Streaming.StreamTransformers)

let get_points_to functions objects globals globals_are_properties (trace: rich_trace) =
  match List.rev trace with
    | [] ->
        CalculatePointsTo.initial_pointsto
          { functions; objects; globals; globals_are_properties;
            function_call = OUndefined; function_apply = OUndefined;
            function_constructor = OUndefined; function_eval = OUndefined;
            object_getPrototypeOf = OUndefined;
            object_setPrototypeOf = OUndefined;
            reflect_getPrototypeOf = OUndefined;
            reflect_setPrototypeOf = OUndefined;
            iids = CCIntMap.empty
          }
    | (_, { points_to }) :: _ -> points_to

let calculate_rich_tracefile
      (funcs, objs, trace, globals, globals_are_properties, iidmap) =
  let trace = ListToRich.enriched_trace_to_rich_trace globals_are_properties trace in
  let points_to = get_points_to funcs objs globals globals_are_properties trace in
  { funcs; objs; globals; globals_are_properties; trace; points_to; iidmap }

let calculate_rich_stream (init: initials) stream =
  StreamToRich.enriched_trace_to_rich_trace init.globals_are_properties stream


let tracefile_to_rich_tracefile
      (functions, objects, trace, globals, globals_are_properties, iids) =
  let initials = { objects; functions; globals; globals_are_properties;
                   function_call = OUndefined; function_apply = OUndefined;
                   function_constructor = OUndefined; function_eval = OUndefined;
                   object_getPrototypeOf = OUndefined;
                   object_setPrototypeOf = OUndefined;
                   reflect_getPrototypeOf = OUndefined;
                   reflect_setPrototypeOf = OUndefined;
                   iids
  } in
  let trace = ListToRich.trace_to_rich_trace initials trace in
    { funcs = functions; objs = objects; globals; globals_are_properties; trace;
      points_to = get_points_to functions objects globals globals_are_properties trace;
      iidmap = iids
    }

let trace_stream_to_rich_stream init stream =
  StreamToRich.trace_to_rich_trace init stream

