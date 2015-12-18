open Types
open Streaming
open TraceTypes

let encode_type isMethod isConstructor = match isMethod, isConstructor with
  | true, true -> ConstructorMethod
  | true, false -> Method
  | false, true -> Constructor
  | false, false -> Function

let check_global locals globals name =
  if List.mem name locals then (locals, globals, false)
  else if List.mem name globals then (locals, globals, true)
  else (locals, name:: globals, true)

let encode_pre ({ f; base; args; isMethod; isConstructor }: raw_funpre) =
  CFunPre { f; base; args; call_type = encode_type isMethod isConstructor }
let encode_post { f; base; args; result; isMethod; isConstructor } =
  CFunPost { f; base; args; result; call_type = encode_type isMethod isConstructor }
let encode_decl { name; value; argument; isCatchParam } =
  assert (argument = None || isCatchParam = false);
  CDeclare { name; value; declaration_type =
                            match argument with
                            | Some i when i >= 0 -> ArgumentBinding i
                            | Some _ -> ArgumentArray
                            | None -> if isCatchParam then CatchParam else Var }

type action =
  | Push of clean_operation
  | Pop of clean_operation
  | AddLocal of string * clean_operation
  | UpdateLocalGlobals of string list * string list * clean_operation
  | Simple of clean_operation
  | Drop

let clean_impl_cases stack locals globals =
  let open Trace in function
    | FunPre (_, fpre) -> Push (encode_pre fpre)
    | FunPost (_, fpost) -> Pop (encode_post fpost)
    | Literal (_, { value; hasGetterSetter }) ->
      Simple (CLiteral { value; hasGetterSetter })
    | ForIn (_, value) -> Simple (CForIn value)
    | Declare (_, decl) -> AddLocal (decl.name, (encode_decl decl))
    | GetFieldPre _ -> Drop
    | GetField (_, { base; offset; value }) ->
      Simple (CGetField { base; offset; value })
    | Read (_, { name; value }) ->
      (* Throw away Jalangi2's isGlobal and isScriptLocal - they turn out to be useless *)
      let (locals', globals', isGlobal) = check_global locals globals name in
      UpdateLocalGlobals (locals', globals', CRead { name; value; isGlobal })
    | PutFieldPre _ -> Drop
    | PutField (_, { base; offset; value }) -> Simple (CPutField { base; offset; value })
    | Write (_, { name; lhs; value }) ->
      let (locals', globals', isGlobal) = check_global locals globals name in
      UpdateLocalGlobals (locals', globals',
                          CWrite { name; lhs; value; isGlobal; isSuccessful = true })
    | Return (_, value) -> Simple (CReturn value)
    | Throw (_, value) -> Simple (CThrow value)
    | With (_, value) -> Simple (CWith value)
    | FunEnter (_, { f; this; args }) -> Simple (CFunEnter { f; this; args })
    | FunExit (_, { ret; exc }) -> Simple (CFunExit { ret; exc })
    | ScriptEnter -> Simple CScriptEnter
    | ScriptExit -> Simple CScriptExit
    | ScriptExc obj -> Simple (CScriptExc obj)
    | BinPre _ -> Drop
    | BinPost (_, { op; left; right; result }) ->
      Simple (CBinary { op; left; right; result })
    | UnaryPre _ -> Drop
    | UnaryPost (_, { op; arg; result }) ->
      Simple (CUnary { op; arg; result })
    | EndExpression _ -> Simple CEndExpression
    | Conditional value -> Simple (CConditional value)

let global_object = OObject 0

let get_object (objects: objects) objval fieldname =
  try
    objects.(get_object objval)
    |> Misc.StringMap.find fieldname
    |> fun { value } -> value
  with Not_found -> OUndefined

let get_object_array objects objval index =
  get_object objects objval (string_of_int index)

let lookup globals (objs: objects) path = match path with
  | [] -> global_object
  | objname :: path ->
    List.fold_left (get_object objs)
      (Misc.StringMap.find objname globals)
      path

let resolve_call objects function_apply function_call f base args call_type =
  let rec resolve f base args argsidx =
    if f = function_apply then
      resolve base
        (get_object_array objects args argsidx)
        (get_object_array objects args (argsidx + 1))
        0
    else if f = function_call then
      resolve base
        (get_object_array objects args argsidx)
        args
        (argsidx + 1)
    else
      { f=f; base=base; args=args; call_type=call_type }
  in resolve f base args 0  

type stackop = Push of funpre option | Keep | Pop | Replace of funpre option | Pop2
let apply_stackop stack = function
  | Push tos -> tos :: stack
  | Keep -> stack
  | Pop -> List.tl stack
  | Pop2 -> List.tl (List.tl stack)
  | Replace tos -> tos :: List.tl stack

let is_instrumented funcs f =
  match f with
  | OFunction (_, fid) ->
    begin match funcs.(fid) with
      | Local { from_jalangi = Some _ } -> true
      | _ -> false
    end
  | _ -> false

let synthesize_events_step funcs op (stack: funpre option list) =
  match op, stack with
  (* drop clearly bad traces *)
  | _, None :: None :: _ ->
    failwith "Trace witnessing function calls in uninstrumented code, should not happen!"
  (* Function exit handling - regular or general exits *)
  (* instrumented -> instrumented *)
  | CFunExit _, Some _ :: Some _ :: _ ->
    (Pop, [ op ])
  (* instrumented -> uninstrumented *)
  | CFunExit { ret = ret; exc =OUndefined},
    Some { f; base=this; args; call_type } :: None :: _ ->
    (Pop, [ op; CFunPost { f; base=this; args; call_type; result = ret } ])
  (* uninstrumented failes with exception into instrumented. Whee! *)
  | CFunExit { ret = OUndefined; exc = exc },
    None :: Some _ :: _ when exc <> OUndefined ->
    (Pop2, [ op; op ])
  (* bad case *)
  | CFunExit _, [] ->
    failwith "Trace witnessing an exit from the toplevel, should not happen!"
  (* uninstrumented -> instrumented *)
  | CFunPost { result }, None :: _ ->
    (Pop, [ CFunExit { ret = result; exc = OUndefined }; op ])
  (* post inside instrumented code *)
  | CFunPost _, Some _ :: _ ->
    (Keep, [ op ])
  (* Function call handling *)
  | CFunPre _, None :: _ ->
    failwith "Trace witnessing a call in uninstrumented code, should not happen!"
  | CFunPre ({ f; base=this; args } as e), _ when is_instrumented funcs f ->
    (Push (Some e), [ op ])
  | CFunPre ({ f; base=this; args }), _ when not (is_instrumented funcs f) ->
    (Push None, [ op; CFunEnter { f; this; args } ])
  | CFunEnter _, [] ->
    failwith "Trace witnessing function entry into toplevel code, should not happen!"
  | CFunEnter _, Some _ :: _ ->
    (Keep, [ op ])
  | CFunEnter { f; this; args }, None :: _ ->
    let call_type = if this = OObject 0 then Function else Method in
    let e = { f; base=this; args; call_type } in
    (Push (Some e), [ CFunPre e; op ])
  (* Function exit handling - exception exits to instrumented code *)
  | CDeclare { declaration_type = CatchParam; value }, None :: _ ->
    (Pop, [ CFunExit { ret = OUndefined; exc = value }; op ])
  | CScriptExc exc, None :: _ ->
    (Pop, [ CFunExit { ret = OUndefined; exc }; op ])
  (* Function exit handling - exception exits to uninstrumented code *)
  | CFunExit _, Some _ :: None :: _ ->
    failwith "Cannot handle exception exits to higher-order code yet"
  (* All cases handled for uninstrumented code *)
  | _, None :: _ ->
    failwith "Unhandled event in uninstrumented code"
  (* All other cases: non-function handling and inside instrumented code, pass through *)
  | _, _ ->
    (Keep, [ op ])

module CleanGeneric = functor(S: Transformers) -> struct
  let clean =
    S.map_list_state ([], ["this"], [])
      (fun (stack, locals, globals) op ->
         match clean_impl_cases stack locals globals op with
         | Push op -> ([op], (locals :: stack, locals, globals))
         | Pop op -> ([op], (List.tl stack, List.hd stack, globals))
         | AddLocal (var, op) -> ([op], (stack, var :: locals, globals))
         | UpdateLocalGlobals (locals', globals', op) ->
           ([op], (stack, locals', globals'))
         | Simple op -> ([op], (stack, locals, globals))
         | Drop -> ([]), (stack, locals, globals))

  let normalize_calls globals (objs: objects) =
    let function_apply = lookup globals objs ["Function"; "prototype"; "apply"]
    and function_call = lookup globals objs ["Function"; "prototype"; "call"] in 
    S.map (function
        | CFunPre { f; base; args; call_type }
          when f = function_apply || f = function_call ->
          CFunPre
            (resolve_call objs function_apply function_call f base args call_type)
        | ev -> ev)

  let synthesize_events funcs trace =
    S.map_list_state []
      (fun stack op ->
         let (stackop, ops') = synthesize_events_step funcs op stack in
         (ops', apply_stackop stack stackop))
      trace

  let remove_use_strict trace =
    let is_use_strict { value; hasGetterSetter } =
      (not hasGetterSetter) && value = OString "use strict" in
    S.map_list_state None
      (fun deferred_literal op ->
         match deferred_literal, op with
         | Some _, CEndExpression -> ([], None)
         | Some l, CLiteral l' when is_use_strict l' -> ([CLiteral l], Some l')
         | Some l, _ -> ([CLiteral l; op], None)
         | None, CLiteral l when is_use_strict l -> ([], Some l)
         | None, _ -> ([op], None))
      trace

  let clean_trace globals funcs objs trace =
    trace
    |> clean
    |> remove_use_strict
    |> normalize_calls globals objs
    |> synthesize_events funcs
end;;

module CleanStream = CleanGeneric(StreamTransformers)
module CleanList = CleanGeneric(ListTransformers)

let clean_tracefile (funs, objs, rawtr, globals, gap) =
  (funs, objs, CleanList.clean_trace globals funs objs rawtr, globals, gap)

let clean_stream data raw =
  let open Reference in
  CleanStream.clean_trace data.globals data.functions data.objects raw

