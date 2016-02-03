open Types
open TraceTypes
type versioned_reference = Reference.versioned_reference

let enrich_step globals_are_properties (op, facts) =
  let mkfieldref base offset =
    Reference.reference_of_field base offset |> LocalFacts.make_versioned facts
  and mkvarref isGlobal name =
    Reference.reference_of_name globals_are_properties facts.aliases isGlobal name
    |> LocalFacts.make_versioned facts in
  let res = match op with
    | CFunPre { f; base; args; call_type } ->
      [RFunPre { f; base; args; call_type } ]
    | CFunPost { f; base; args; result; call_type } -> [RFunPost { f; base; args; result; call_type }]
    | CLiteral { value; hasGetterSetter } -> [RLiteral { value; hasGetterSetter }]
    | CForIn value -> [RForIn value]
    | CDeclare { name; value; declaration_type = ArgumentBinding idx } ->
      if StringMap.mem name facts.aliases then
        let ref = StringMap.find name facts.aliases
                        |> Reference.reference_of_fieldref
                        |> LocalFacts.make_versioned facts
        in
          [RAlias { name; ref; source = Argument idx };
           RWrite { ref; oldref = ref; value; success = true } ]
      else
        let ref = Reference.reference_of_local_name name |> LocalFacts.make_versioned facts in
        [RLocal { name; ref };
         RWrite { ref; oldref = ref; value = OUndefined; success = true } ]
    | CDeclare { name; value; declaration_type = CatchParam } ->
      let ref = Reference.reference_of_local_name name |> LocalFacts.make_versioned facts in
      [RCatch { name; ref };
       RWrite { ref; oldref = ref; value; success = true } ]
    | CDeclare { name; value } ->
      let ref = Reference.reference_of_local_name name |> LocalFacts.make_versioned facts in
      [RLocal { name; ref };
       RWrite { ref; oldref = ref; value; success = true } ]
    | CGetFieldPre _ -> Debug.debug "Unexpected get_field_pre@."; []
    | CPutFieldPre _ -> Debug.debug "Unexpected put_field_pre@."; []
    | CGetField { base; offset; value } ->
      [RRead { ref = mkfieldref base offset; value }]
    | CPutField { base; offset; value } ->
      (* FIXME success handling *)
      [RWrite {
          ref = mkfieldref base offset;
          oldref = Misc.Option.some facts.last_update;
          value; success = true
        }]
    | CRead { name; value; isGlobal } ->
      [RRead { ref = mkvarref isGlobal name; value }]
    | CWrite { name; lhs; value; isGlobal } ->
      (* FIXME success handling *)
      [RWrite {
          ref = mkvarref isGlobal name;
          oldref = Misc.Option.some facts.last_update;
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
  let to_rich_tracefile globals_are_properties =
    S.map_list (enrich_step globals_are_properties)
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

let calculate_rich_stream init stream =
  StreamEnrich.to_rich_tracefile init.Reference.globals_are_properties stream

let tracefile_to_rich_tracefile trace =
  trace
    |> CleanTrace.clean_tracefile
    |> LocalFacts.collect_arguments_tracefile
    |> CalculateVersions.collect_versions_trace
    |> CalculatePointsTo.calculate_pointsto
    |> calculate_rich_tracefile

let trace_stream_to_rich_stream init stream =
  stream
    |> CleanTrace.clean_stream init
    |> LocalFacts.collect_arguments_stream
    |> CalculateVersions.collect_versions_stream init
    |> CalculatePointsTo.collect_pointsto_stream init
    |> calculate_rich_stream init
