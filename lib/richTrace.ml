open Types
open TraceTypes
open LocalFacts
type versioned_reference = Reference.versioned_reference

let enrich_step globals_are_properties (op, facts) =
  let mkfieldref base offset =
    Reference.reference_of_field base offset |> LocalFacts.make_versioned facts
  and mkvarref name =
    Reference.reference_of_name globals_are_properties facts.names name
    |> LocalFacts.make_versioned facts in
  let res = match op with
    | CFunPre { f; base; args; call_type } ->
      [RFunPre { f; base; args; call_type } ]
    | CFunPost { f; base; args; result; call_type } -> [RFunPost { f; base; args; result; call_type }]
    | CLiteral { value; hasGetterSetter } -> [RLiteral { value; hasGetterSetter }]
    | CForIn value -> [RForIn value]
    | CDeclare { name; value; declaration_type = ArgumentBinding idx } ->
      begin match StringMap.find name facts.names with
        | Reference.Field ref ->
            let ref = StringMap.find name facts.names |> LocalFacts.make_versioned facts
            in
              [RAlias { name; ref; source = Argument idx };
               RWrite { ref; oldref = ref; value; success = true } ]
        | Reference.Variable _ ->
            let ref = Reference.reference_of_name globals_are_properties facts.names name
              |> LocalFacts.make_versioned facts in
              [RLocal { name; ref };
               RWrite { ref; oldref = ref; value = OUndefined; success = true } ]
      end
    | CDeclare { name; value; declaration_type = CatchParam } ->
      let ref = Reference.reference_of_name globals_are_properties facts.names name |> LocalFacts.make_versioned facts in
      [RCatch { name; ref };
       RWrite { ref; oldref = ref; value; success = true } ]
    | CDeclare { name; value } ->
      let ref = Reference.reference_of_name globals_are_properties facts.names name |> LocalFacts.make_versioned facts in
      [RLocal { name; ref };
       RWrite { ref; oldref = ref; value; success = true } ]
    | CGetFieldPre _ -> Log.debug (fun m -> m "Unexpected get_field_pre"); []
    | CPutFieldPre _ -> Log.debug (fun m -> m "Unexpected put_field_pre"); []
    | CGetField { base; offset; value } ->
      [RRead { ref = mkfieldref base offset; value }]
    | CPutField { base; offset; value } ->
      (* FIXME success handling *)
      [RWrite {
          ref = mkfieldref base offset;
          oldref = BatOption.get facts.last_update;
          value; success = true
        }]
    | CRead { name; value } ->
      [RRead { ref = mkvarref name; value }]
    | CWrite { name; lhs; value } ->
      (* FIXME success handling *)
      [RWrite {
          ref = mkvarref name;
          oldref = BatOption.get facts.last_update;
          value;
          success = true
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
    | CConditional value -> [RConditional value] in
  List.map (fun op -> (op, facts)) res

module GenericEnrich(S: Streaming.Transformers) = struct
  let to_rich_tracefile globals_are_properties trace =
    trace
      |> S.map_list (enrich_step globals_are_properties)
      |> S.map (fun (op, ({ last_update; versions; points_to }: local_facts)) ->
                   (op, {last_update; versions; points_to }))
end;;

module ListEnrich = GenericEnrich(Streaming.ListTransformers)
module StreamEnrich = GenericEnrich(Streaming.StreamTransformers)

let calculate_rich_tracefile
      (funcs, objs, trace, globals, globals_are_properties) =
  let trace = ListEnrich.to_rich_tracefile globals_are_properties trace in
  let points_to = match List.rev trace with
    | [] ->
        let open Reference in
        CalculatePointsTo.initial_pointsto
          { functions = funcs; objects = objs; globals; globals_are_properties }
    | (_, { points_to }) :: _ -> points_to
  in
  { funcs; objs; globals; globals_are_properties; trace; points_to }

let calculate_rich_stream (init: initials) stream =
  StreamEnrich.to_rich_tracefile init.globals_are_properties stream


let tracefile_to_rich_tracefile trace =
  let module E = EnrichTrace.Make(Streaming.ListTransformers)
  in let do_enrichment
           (functions, objects, trace, globals, globals_are_properties) =
    (functions, objects,
     E.collect { functions; objects; globals; globals_are_properties } trace,
     globals, globals_are_properties)
  in
    trace
    |> CleanTrace.clean_tracefile
    |> do_enrichment
    |> calculate_rich_tracefile

let trace_stream_to_rich_stream init stream =
  let module E = EnrichTrace.Make(Streaming.StreamTransformers) in
  stream
    |> CleanTrace.clean_stream init
    |> E.collect init
    |> calculate_rich_stream init
