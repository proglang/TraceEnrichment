open TypesJS
open Streaming
open TraceTypes

let encode_type isMethod isConstructor = match isMethod, isConstructor with
  | true, true ->
      Log.debug (fun m -> m "Treating 'method constructor' as constructor");
      Constructor
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

let clean_impl_cases initials sid =
  let open Trace in function
    | SwitchScript sid -> ([], sid)
    | FunPre (_, fpre) -> ([encode_pre fpre, sid], sid)
    | FunPost (_, fpost) -> ([encode_post fpost, sid], sid)
    | Literal (_, { value; hasGetterSetter }) ->
      ([CLiteral { value; hasGetterSetter }, sid], sid)
    | ForIn (_, value) -> ([CForIn value, sid], sid)
    | Declare (_, decl) -> ([encode_decl decl, sid], sid)
    | GetFieldPre (_, { base; offset }) -> ([CGetFieldPre (base, offset), sid], sid)
    | GetField (_, { base; offset; value; isComputed }) ->
      ([CGetField { base; offset; value; isComputed; actual_base = base }, sid], sid)
    | Read (_, { name; value }) ->
      ([CRead { name; value }, sid], sid)
    | PutFieldPre (_, { base; offset; value; isComputed }) ->
        ([CPutFieldPre { base; offset; value; isComputed; actual_base = base }, sid], sid)
    | PutField (_, { base; offset; value; isComputed }) ->
        ([CPutField { base; offset; value; isComputed; actual_base = base }, sid], sid)
    | Write (_, { name; lhs; value }) ->
        ([CWrite { name; lhs; value; isSuccessful = true }, sid], sid)
    | Return (_, value) -> ([CReturn value, sid], sid)
    | Throw (_, value) -> ([CThrow value, sid], sid)
    | With (_, value) -> ([CWith value, sid], sid)
    | FunEnter (_, { f; this; args }) -> ([CFunEnter { f; this; args }, sid], sid)
    | FunExit (_, { ret; exc }) -> ([CFunExit { ret; exc }, sid], sid)
    | ScriptEnter -> ([CScriptEnter, sid], sid)
    | ScriptExit -> ([CScriptExit, sid], sid)
    | ScriptExc obj -> ([CScriptExc obj, sid], sid)
    | BinPre _ -> ([], sid)
    | BinPost (_, { op; left; right; result }) ->
      ([CBinary { op; left; right; result }, sid], sid)
    | UnaryPre _ -> ([], sid)
    | UnaryPost (_, { op; arg; result }) ->
      ([CUnary { op; arg; result }, sid], sid)
    | EndExpression _ -> ([CEndExpression, sid], sid)
    | Conditional (iid, value) ->
        let loc =
          match CCIntMap.find sid initials.iids with
            | Some iidmap -> CCIntMap.find iid iidmap
            | None -> None
        in ([CConditional (loc, value), sid], sid)

let global_object = OObject 0

let get_object_array ?(required=false) objects objval index =
  lookup_object objects objval (string_of_int index) ~required

let has_field objs obj fld =
  StringMap.mem fld (BatDynArray.get objs (get_object obj))
let has_index objs obj idx = has_field objs obj (string_of_int idx)

let debug_get_array objects base =
  try
    let rec get n objs =
      if has_index objects base n then
        get (n+1) (get_object_array ~required:true objects base n :: objs)
      else
        List.rev objs
    in get 0 []
  with ((ObjectNotFound (objid, fld)) as e) ->
    Log.debug (fun m -> m "Not a proper array: %a, containing @[<hov 2>%a@]@ (Can't find %s in %a)"
                          pp_jsval base
                          pp_objectspec (BatDynArray.get objects (get_object base))
                          fld pp_objectid objid
    );
    raise e
      
let resolve_call objects function_apply function_call f base args call_type =
  Log.debug (fun m -> m "Resolving call to %s with arguments %a"
    (if f = function_apply then "apply"
     else if f = function_call then "call"
     else "some random function")
    (Fmt.list pp_jsval) (debug_get_array objects args));
  let rec resolve f base args argsidx =
    if f = function_apply then
      resolve base
        (get_object_array ~required:true objects args argsidx)
        (get_object_array ~required:true objects args (argsidx + 1))
        0
    else if f = function_call then
      resolve base
        (get_object_array ~required:true objects args argsidx)
        args
        (argsidx + 1)
    else
      { f=f; base=base; args=args; call_type=call_type }
  in try
    resolve f base args 0  
  with
      ObjectNotFound _ ->
        Log.debug (fun m -> m "Cannot resolve call due to objects not being found");
        { f=f; base=base; args=args; call_type = call_type }

type 'a stackop = Push of 'a | Keep | Pop | Replace of 'a | Pop2 | PopReplace of 'a | PushReplace of 'a * 'a | Pop2Replace of 'a
let apply_stackop pp_value stack = function
  | Push tos -> Log.debug (fun m -> m "Stack: Pushing %a" pp_value tos); tos :: stack
  | Keep -> stack
  | Pop -> Log.debug (fun m -> m "Stack: Popping"); List.tl stack
  | Pop2 -> Log.debug (fun m -> m "Stack: Popping twice"); List.tl (List.tl stack)
  | Replace tos -> Log.debug (fun m -> m "Stack: Replacing TOS with %a" pp_value tos); tos :: List.tl stack
  | PopReplace tos -> Log.debug (fun m -> m "Stack: Popping and replacing TOS with %a" pp_value tos); tos :: List.tl (List.tl stack)
  | PushReplace (tos1, tos2) -> Log.debug (fun m -> m "Stack: Pusing %a and %a" pp_value tos1 pp_value tos2); tos1 :: tos2 :: List.tl stack
  | Pop2Replace tos -> Log.debug (fun m -> m "Stack: Popping twice and replacing TOS with %a" pp_value tos); tos :: List.tl (List.tl (List.tl stack))

let is_internal funcs f =
  match f with
  | OFunction (_, fid) ->
    begin match BatDynArray.get funcs fid with
      | OrigCode _ -> true
      | _ -> false
    end
  | _ -> false

let pp_funpre pp ({ f }: funpre) = pp_jsval pp f
type func_type =
  | IntFunc of funpre
  | ExtFunc of jsval
  | ExtFuncExc of jsval * jsval
  | EventFunc of funpre
  [@@deriving show]

let make_silent_catch exc =
  CDeclare { name = ""; value = exc;
             declaration_type = CatchParam }
let make_funpre ({ f; this; args }: funenter) =
  { f; base=this; args; call_type = Method }

let synthesize_events_step funcs (op, sid) stack =
  Log.debug (fun m -> m "Matching %a with %a"
                        pp_clean_operation op
                        (Fmt.list pp_func_type) stack);
  let (cmd, ops) = match op, stack with
  | _, (ExtFunc _ | ExtFuncExc _) :: (ExtFunc _ | ExtFuncExc _) ::_
  | _, EventFunc _ :: _ :: _ ->
      failwith "Bad stack"
  | CFunPre ({ f; base; args } as funpre), (IntFunc _ :: _ | EventFunc _ :: _ | []) ->
      if is_internal funcs f then
        (Push (IntFunc funpre), [ op ])
      else
        (Push (ExtFunc f), [ op; CFunEnter { f; this=base; args }])
  | CFunPre _, (ExtFunc _ | ExtFuncExc _) :: _ ->
      failwith "pre seen in external code"
  | CFunPost { f }, ((IntFunc { f = f' } :: _)) ->
      if is_internal funcs f then
        (Keep, [ op ])
      else
        failwith (Fmt.strf "post for external function %a with internal top-of-stack %a"
                    pp_jsval f pp_jsval  f')
  | CFunPost { f }, ((EventFunc { f = f' } :: _)) ->
      if is_internal funcs f then
        (Keep, [ op ])
      else
        failwith (Fmt.strf "post for external function %a with event top-of-stack %a"
                    pp_jsval f pp_jsval  f')
  | CFunPost { f }, [] ->
      if is_internal funcs f then
        (Keep, [ op ])
      else
        failwith (Fmt.strf "post for external function %a at top level" pp_jsval f)
  | CFunPost { f; result }, ExtFunc f' :: _ ->
      if f <> f' then
        failwith "post for an unexpected function"
      else if is_internal funcs f then
        failwith "internal code treated as external"
      else
        (Pop, [ CFunExit { ret = result; exc = OUndefined }; op ])
  | CFunPost { f; result }, ExtFuncExc (f', exc) :: _ ->
      if f <> f' then
        failwith "post for an unexpected function"
      else if is_internal funcs f then
        failwith "internal code treated as external"
      else
        (Pop, [ make_silent_catch exc;
                CFunExit { ret = result; exc = OUndefined };
                op ])
  | CFunEnter { f }, IntFunc { f=f' } :: _ ->
      if f <> f' then
        failwith "enter for an unexpected function"
      else
        (Keep, [ op ])
  | CFunEnter funenter, ExtFunc _ :: _ ->
      let funpre = make_funpre funenter in
      (Push (IntFunc funpre), [ CFunPre funpre; op ])
  | CFunEnter funenter, ExtFuncExc (f, exc) :: _ ->
      let funpre = make_funpre funenter in
      (PushReplace (IntFunc funpre, ExtFunc f),
       [ make_silent_catch exc; CFunPre funpre; op ])
  | CFunEnter _, EventFunc _ :: _ ->
      failwith "Entry to an event function"
  | CFunEnter funenter, [] ->
      let funpre = make_funpre funenter in
        (Push (EventFunc funpre), [ CFunPre funpre; op ])
  | CFunExit { exc = OUndefined; ret }, IntFunc _ :: (IntFunc _ :: _ | EventFunc _ :: _ | []) ->
      (Pop, [ op ])
  | CFunExit { exc = OUndefined; ret }, [ EventFunc { f; base; args } ] ->
      (Pop, [ op; CFunPost { f; base; args; result = ret; call_type = Function } ])
  | CFunExit { exc = OUndefined; ret }, IntFunc { f; base; args } :: ExtFunc _ :: _ ->
      (Pop, [ op; CFunPost { f; base; args; result = ret; call_type = Method } ])
  | CFunExit { exc = OUndefined }, IntFunc _ :: ExtFuncExc _ :: _ ->
      failwith "exit into external error frame"
  | CFunExit { exc = OUndefined }, [] ->
      failwith "exit in the top frame"
  | CFunExit { exc = OUndefined }, (ExtFunc _ | ExtFuncExc _) :: _ ->
      failwith "exit seen in external frame"
  | CFunExit { ret = OUndefined; exc }, IntFunc _ :: (IntFunc _ :: _ | EventFunc _ :: _ | []) ->
      (Pop, [ op ])
  | CFunExit { ret = OUndefined; exc }, [ EventFunc { f; base; args } ] ->
      (Pop, [ op; make_silent_catch exc ])
  | CFunExit { ret = OUndefined; exc }, IntFunc _ :: ExtFunc f :: _ ->
      (PopReplace (ExtFuncExc (f, exc)), [ op ])
  | CFunExit { ret = OUndefined }, IntFunc _ :: ExtFuncExc _ :: _ ->
      failwith "exit into external error frame"
  | CFunExit { ret = OUndefined }, [] ->
      failwith "exit in the top frame"
  | CFunExit { ret = OUndefined; exc }, ExtFunc _ :: IntFunc _ :: ExtFunc f :: _ ->
      (Pop2Replace (ExtFuncExc (f, exc)), [ CThrow exc; op; op ])
  | CFunExit { ret = OUndefined; exc }, ExtFuncExc (_, exc') :: IntFunc _ :: ExtFunc f :: _ ->
      if exc = exc' then
        (Pop2Replace (ExtFuncExc (f, exc)), [ op; op ])
      else
        (Pop2Replace (ExtFuncExc (f, exc)), [ make_silent_catch exc'; CThrow exc; op; op ])
  | CFunExit { ret = OUndefined; exc }, ExtFunc _ :: _ ->
      (Pop2, [ CThrow exc; op; op ])
  | CFunExit { ret = OUndefined; exc }, ExtFuncExc (_, exc') :: _ ->
      if exc = exc' then
        (Pop2, [ op; op ])
      else
        (Pop2, [ make_silent_catch exc'; CThrow exc; op; op])
  | CFunExit _, _ ->
      failwith "invalid exit: both return value and exception"
  | _, (IntFunc _ :: _ | [])
  | _, EventFunc _ :: [] ->
      (Keep, [ op ])
  | CScriptExc exc, ExtFunc f :: _ ->
      (Pop, [ CThrow exc; CFunExit { ret = OUndefined; exc }; op ])
  | CScriptExc exc, ExtFuncExc (f, exc') :: _ ->
      if exc = exc' then
        (Pop, [ CFunExit { exc; ret = OUndefined }; op ])
      else
        (Pop, [ make_silent_catch exc'; CThrow exc; CFunExit { exc; ret = OUndefined }; op ])
  | CDeclare { value = exc; declaration_type= CatchParam }, ExtFunc f :: _ ->
      (Pop, [ CThrow exc; CFunExit { ret = OUndefined; exc }; op ])
  | CDeclare { value = exc; declaration_type= CatchParam }, ExtFuncExc (f, exc') :: _ ->
      if exc = exc' then
        (Pop, [ CFunExit { exc; ret = OUndefined }; op ])
      else
        (Pop, [ make_silent_catch exc'; CThrow exc; CFunExit { exc; ret = OUndefined }; op ])
  | _, (ExtFunc _ | ExtFuncExc _) :: _ ->
      failwith "Bad event in external frame"
  in (cmd, List.map (fun op -> (op, sid)) ops)

type clean_level = SynthesizeEvents | Normalizations | SynthesizeGettersAndSetters | JustClean

module CleanGeneric = functor(S: Transformers) -> struct
  let clean initials = S.map_list_state 0 (clean_impl_cases initials)

  let normalize_calls initials =
    Log.debug (fun m -> m "Normalizing calls");
    S.map (fun (op, sid) -> ((match op with
        | CFunPre { f; base; args; call_type }
          when f = initials.function_apply || f = initials.function_call ->
          CFunPre
            (resolve_call initials.objects initials.function_apply initials.function_call
               f base args call_type)
        | CFunPost { f; base; args; call_type; result }
          when f = initials.function_apply || f = initials.function_call ->
            let ({ f; args; base; call_type }: funpre) =
              resolve_call initials.objects initials.function_apply initials.function_call
                f base args call_type
            in CFunPost { f; base; args; call_type; result }
        | ev -> ev), sid))

  let normalize_function_constructor initials =
    Log.debug (fun m -> m "Normalizing function constructor");
    S.map_list_state false
      (fun in_constructor (op, sid) ->
         match in_constructor, op with
           | true, CScriptEnter -> ([op, sid], true)
           | true, CScriptExit -> ([op, sid], true)
           | true, CEndExpression -> ([op, sid], true)
           | true, CLiteral _ -> ([op, sid], true)
           | true, CDeclare { declaration_type = CatchParam; value } ->
               ([CThrow value, sid; CFunExit { exc=value; ret=OUndefined }, sid; op, sid], false)
           | true, CScriptExc exc ->
               ([CThrow exc, sid; CFunExit { exc; ret=OUndefined }, sid; op, sid], false)
           | true, CFunExit { exc; ret=OUndefined } when exc <> OUndefined ->
               ([CThrow exc, sid; op, sid; op, sid], false)
           | true, CFunPost { result = value } ->
               ([CReturn value, sid; CFunExit { ret=value; exc=OUndefined }, sid; op, sid], false)
           | true, _ ->
               prerr_endline "Bad event in a Function constructor body";
               ([op, sid], true)
           | false, CFunPre { f; base; args } when f = initials.function_constructor ->
               ([op, sid; CFunEnter { f; this=base; args }, sid], true)
           | false, _ -> ([op, sid], false))

  (* This bit involves quite some rewriting. What we do:
   * - Writes to/reads from __proto__ become writes to/reads from prototype.
   * - Object.(get|set)PrototypeOf are translated to prototype reads and writes.
   * The same is done with Reflect operations. *)
  type prototype_mode =
      ObjectGetProto of jsval
    | ObjectSetProto of jsval * jsval
    | ReflectGetProto of jsval
    | ReflectSetProto of jsval * jsval
    | NonePrototype
  let normalize_prototype_manipulation initials =
    Log.debug (fun m -> m "Normalizing prototype manipulation");
    let cases = [
      (initials.object_getPrototypeOf,
       fun args -> let obj = get_object_array initials.objects args 0
       in (ObjectGetProto obj));
      (initials.reflect_getPrototypeOf,
       fun args -> let obj = get_object_array initials.objects args 0
       in (ReflectGetProto obj));
      (initials.object_setPrototypeOf,
       fun args ->
         let obj = get_object_array initials.objects args 0
         and pro = get_object_array initials.objects args 1
         in (ObjectSetProto (obj, pro)));
      (initials.reflect_setPrototypeOf,
       fun args ->
         let obj = get_object_array initials.objects args 0
         and pro = get_object_array initials.objects args 1
         in (ReflectSetProto (obj, pro)))
    ]
    in S.map_list_state NonePrototype
      (fun mode (op, sid) -> let (op, mode) = match op, mode with
         | CFunEnter { f; args }, NonePrototype ->
             begin try ([op], List.assoc f cases args)
             with Not_found -> ([op], NonePrototype)
             end
         | CGetField ({ offset = "_proto__" } as acc), NonePrototype ->
             ([CGetField {acc with offset = "prototype"}], NonePrototype)
         | CPutField ({ offset = "_proto__" } as acc), NonePrototype ->
             ([CPutField {acc with offset = "prototype"}], NonePrototype)
         | _, NonePrototype -> ([op], NonePrototype)
         | CFunExit { ret; exc = OUndefined }, ObjectGetProto obj ->
             ([CGetField { base = obj; actual_base = obj;
                           offset = "prototype"; value = ret;
                           isComputed = false };
               op],
              NonePrototype)
         | CFunExit { ret; exc = OUndefined }, ReflectGetProto obj ->
             ([CGetField { base = obj; actual_base = obj;
                           offset = "prototype"; value = ret;
                           isComputed = false };
               op],
              NonePrototype)
         | CFunExit { exc = OUndefined }, ObjectSetProto (obj, pro) ->
             ([CPutField { base = obj; actual_base = obj;
                           offset = "prototype"; value = pro;
                           isComputed = false };
               op],
              NonePrototype)
         | CFunExit { exc = OUndefined }, ReflectSetProto (obj, pro) ->
             ([CPutField { base = obj; actual_base = obj;
                           offset = "prototype"; value = pro;
                           isComputed = false };
               op],
              NonePrototype)
         | CFunExit { exc }, _ -> ([CThrow exc; op], NonePrototype)
         | CThrow _, _ -> ([op], NonePrototype)
         | _, _ ->
             Log.err (fun m ->
                        m "Operation %a encountered when expecting prototype updates"
                          pp_clean_operation op);
             ([op], NonePrototype)
       in ((List.map (fun op -> (op, sid)) op, mode)))
  (* Note that, for once, this is not a function call stack, but a eval context stack. *)
  let normalize_eval_step eval (frame_stack, special, last_call) (op, sid) =
    let (ops, state) = match frame_stack, op with
    | true :: frames, CScriptExit -> ([op], (frames, true, None))
    | true :: frames, CScriptExc exc -> ([op; CFunExit { ret=OUndefined; exc }], (frames, false, None))
    | frames, CScriptEnter ->
        begin match special, last_call with
          | true, Some (f, this, args) ->
              ([CFunEnter {f; this; args}; op], (true::frames, false, None))
          | false, _ -> ([op], (false:: frames, false, None))
          | true, None ->
              begin
                prerr_endline "Cannot synthesize proper entry to eval, no call!";
                ([CFunEnter { f=eval; this=OObject 0; args=OObject 0}; op], (true::frames, false, None))
              end
        end
    | false :: frames, CScriptExit -> ([op], (frames, false, None))
    | false :: frames, CScriptExc exc -> ([op], (frames, false, None))
    | [], CScriptExit
    | [], CScriptExc _ ->
        prerr_endline "Exiting from a script that has not been entered";
        ([op], ([], false, None))
    | frames, CFunPost { result } ->
        if special then
          ([CFunExit { ret = result; exc = OUndefined }; op], (frames, false, None))
        else
          ([op], (frames, false, None))
    | frames, CFunPre { f; base; args } when f = eval ->
        if special then
          prerr_endline "Unexpected special handling request when calling eval";
        ([op], (frames, true, Some (f, base, args)))
    | frames, op ->
        if special then
          prerr_endline "Unexpected special handling request when handling regular event";
        ([op], (frames, false, None))
    in (List.map (fun op -> (op, sid)) ops, state)

  let normalize_eval initials =
    Log.debug (fun m -> m "Normalizing eval constructor");
    (*
    let eval =  in
     *)
    S.map_list_state ([], false, None)
      (fun state op -> normalize_eval_step initials.function_eval state op)

  let normalize_string_subscripts_step = function
    | CGetFieldPre (OString _, _), _ -> []
    | CPutFieldPre { base = OString _ }, _ -> []
    | CGetField  { base = (OString _) as left; offset; value }, sid ->
        [ CBinary { op = "_string_subscript"; left; right = OString offset; result = value }, sid ]
    | CPutField { base =  OString _; offset; value }, _ ->
        [] (* Thanks, JavaScript - string assignment is a no-op *)
    | event -> [event]

  let normalize_string_subscripts =
    S.map_list normalize_string_subscripts_step

  let synthesize_events funcs trace =
    S.map_list_state []
      (fun stack op ->
         let (stackop, ops') = synthesize_events_step funcs op stack in
         (ops', apply_stackop pp_func_type stack stackop))
      trace

  let remove_use_strict trace =
    let is_use_strict { value; hasGetterSetter } =
      (not hasGetterSetter) && value = OString "use strict" in
    S.map_list_state None
      (fun deferred_literal (op, sid) ->
         match deferred_literal, op with
         | Some _, CEndExpression -> ([], None)
         | Some l, CLiteral l' when is_use_strict l' -> ([CLiteral l, sid], Some l')
         | Some l, _ -> ([CLiteral l, sid; op, sid], None)
         | None, CLiteral l when is_use_strict l -> ([], Some l)
         | None, _ -> ([op, sid], None))
      trace

  type mode = Neither | Read | Write
  let synthesize_getters_and_setters_step (op, sid) mode stack =
    match op, mode, stack with
      | CGetFieldPre _, _, _ -> (Keep, Read, [])
      | CPutFieldPre _, _, _ -> (Keep, Write, [])
      | CFunEnter { f; this; args }, Read, _ ->
          (Push (Some (f, this, args)), Neither,
           [CFunPre { f; base=this; args; call_type = Method }, sid; op, sid])
      | CFunEnter { f; this; args }, Write, _ ->
          (Push (Some (f, this, args)), Neither,
           [CFunPre { f; base=this; args; call_type = Method }, sid; op, sid])
      | CFunEnter _, Neither, _ -> (Push None, Neither, [op, sid])
      | CFunExit { ret; exc=OUndefined }, _, Some (f, base, args) :: _ ->
          (Pop, Neither,
           [op, sid; CFunPost { f; base; args; call_type = Method; result=ret }, sid])
      | CFunExit _, _, _ -> (Pop, Neither, [op, sid])
      | _, _, _ -> (Keep, Neither, [op, sid])

  let pp_gs pp = function
    | Some (f, this, args) ->
        Format.fprintf pp "f=%a, this=%a, args=%a" pp_jsval f pp_jsval this pp_jsval args
    | None -> Format.fprintf pp "(none)"

  let synthesize_getters_and_setters trace =
    S.map_list_state (Neither, [])
      (fun (mode, stack) op ->
         let (stackop, mode, ops') = synthesize_getters_and_setters_step op mode stack in
           (ops', (mode, apply_stackop pp_gs stack stackop)))
      trace

  type exit_type = None | Value | Exception
  type validate_state = {
    stack: (jsval * jsval * jsval * jsval option * call_type) list;
    saw_use_strict: bool;
    saw_fun_pre: bool;
    saw_fun_exit: exit_type;
  }

  type validate_level = Basic | NoUseStrict | NormalizedCalls | SynthesizedEvents
  let validate_step level function_call function_apply (op, _) state =
    let eqval v1 v2 =
      match v1, v2 with
        | ONumberFloat f1, ONumberFloat f2 ->
            f1 = f2 || (classify_float f1 = FP_nan && classify_float f2 = FP_nan)
        | _, _ -> v1 = v2
    in let is_function = function
      | OFunction _ -> ()
      | v -> Format.eprintf "Not a function: %a@." pp_jsval v
    and is_object = function
      | OObject _ | OFunction _ | OOther _ -> ()
      | v -> Format.eprintf "Not an object: %a@." pp_jsval v
    and is_top_f f = match state with
      | { stack = (f', _, _, _, _) :: _ } when f = f' -> ()
      | { stack = (f', _, _, _, _) :: _ } ->
          Format.eprintf "Top-of-stack f is %a, not %a@." pp_jsval f' pp_jsval f
      | _ -> Format.eprintf "Top-of-stack f is not %a, stack empty@." pp_jsval f
    and is_top_base_enter base = match state with
      | { stack = (_, OObject 0, _, _, Constructor) :: _ }-> ()
      | { stack = (_, (OUndefined | ONull | OBoolean _ | ONumberInt _ | ONumberFloat _ | OString _ | OSymbol _), _, _, _) :: _ }-> ()
      | { stack = (_, base', _, _, _) :: _ } when eqval base base' -> ()
      | { stack = (_, base', _, _, _) :: _ } ->
          Format.eprintf "Top-of-stack base is %a, not %a@." pp_jsval base' pp_jsval base
      | _ -> Format.eprintf "Top-of-stack base is not %a, stack empty@." pp_jsval base
    and is_top_args args = match state with
      | { stack = (_, _, args', _, _) :: _ } when eqval args args' -> ()
      | { stack = (_, _, args', _, _) :: _ } ->
          Format.eprintf "Top-of-stack args is %a, not %a@." pp_jsval args' pp_jsval args
      | _ -> Format.eprintf "Top-of-stack args is not %a, stack empty@." pp_jsval args
    and is_top_result result = match state with
      | { stack = (_, _, _, Some result', _) :: _ } when eqval result result' -> ()
      | { stack = (_, _, _, Some result', _) :: _ } ->
          Format.eprintf "Top-of-stack result is %a, not %a@." pp_jsval result' pp_jsval result
      | { stack = (_, _, _, None, _) :: _ } ->
          Format.eprintf "Top-of-stack result is not %a, result unknown" pp_jsval result
      | _ -> Format.eprintf "Top-of-stack result is not %a, stack empty@." pp_jsval result
    and is_top_call_type call_type = match state with
      | { stack = (_, _, _, _, call_type') :: _ } when call_type = call_type' -> ()
      | { stack = (_, _, _, _, call_type') :: _ } ->
          Format.eprintf "Top-of-stack call type is %a, not %a@." pp_call_type call_type' pp_call_type call_type
      | _ -> Format.eprintf "Top-of-stack call type is not %a, stack empty@." pp_call_type call_type
    and is_normalized f =
      if f = function_call || f = function_apply then begin
        Format.eprintf "Unnormalized call@."
      end
    and calls_stacked = level = SynthesizedEvents
    and calls_normalized =
      level = SynthesizedEvents || level = NormalizedCalls
    in
      Log.debug (fun m -> m "Operation: %a" pp_clean_operation op);
    let state' = match op with
    | CFunPre { f; base; args; call_type } ->
        is_function f;
        (* Note that we don't check if base is an object.
         * Why? Because we can Function.call with a non-object
         * base. Bizzare but legal :) *)
        is_object args;
        begin if calls_normalized then is_normalized f end;
        { state with stack = (f, base, args, None, call_type) :: state.stack }
    | CFunPost { f; base; args; call_type; result } ->
        if calls_stacked then begin
          is_top_f f;
          is_top_args args;
          is_top_call_type call_type;
          match state with
            | { stack = (_, _, _, _, Constructor) :: _ } -> ()
            | _ -> is_top_result result
        end;
        { state with stack = List.tl state.stack }
    | CFunEnter { f; this; args } ->
        if calls_stacked then begin
          is_top_f f;
          is_top_base_enter this;
          (* args is different *)
        end;
        state
    | CFunExit { exc = OUndefined; ret } ->
        begin
          match state.stack with
            | (f, this, args, None, call_type) :: stack ->
                { state with stack = (f, this, args, Some ret, call_type) :: stack }
            | _ :: _ ->
                if calls_stacked then
                  prerr_endline "Double function exit";
                state
            | [] ->
                if calls_stacked then
                  prerr_endline "Exit from top level";
                state
        end
      | CFunExit { exc; ret = OUndefined } ->
          { state with stack = List.tl state.stack }
      | CFunExit _ ->
          prerr_endline "Exit with both value and exception@.";
          { state with stack = List.tl state.stack }
      | CLiteral _ -> state (* use strict handling is seperate *)
      | CForIn value -> is_object value; state
      | CDeclare _ -> state (* no clean-up has been done here *)
      | CGetField _ -> state (* check later? *)
      | CPutField _ -> state (* check later? *)
      | CGetFieldPre _ ->
          if calls_stacked then
            prerr_endline "GetFieldPre still remaining";
          state
      | CPutFieldPre _ ->
          if calls_stacked then
            prerr_endline "PutFieldPre still remaining";
          state
      | CRead _ -> state (* check later? *)
      | CWrite _ -> state (* check later? *)
      | CReturn _ -> state
      | CThrow OUndefined -> prerr_endline "Throwing an undefined value";
                             state
      | CThrow _ -> state
      | CWith v -> is_object v; state
      | CScriptEnter -> state
      | CScriptExit -> state
      | CScriptExc _ -> state
      | CBinary _ -> state
      | CUnary _ -> state
      | CEndExpression -> state
      | CConditional _ -> state
    in let state' = match op with
      | CLiteral { value = OString "use strict" } ->
          { state' with saw_use_strict = true }
      | CEndExpression ->
          begin 
            if level <> Basic && state'.saw_use_strict then begin
              prerr_endline "use strict found"
            end
          end;
          { state' with saw_use_strict = false }
      | _ ->
          { state' with saw_use_strict = false }
    in let state' = match op with
      | CFunPre _ ->
          if state'.saw_fun_pre then
            prerr_endline "Double funpre encountered";
          { state' with saw_fun_pre = true }
      | CFunEnter _ ->
          if calls_stacked && not state'.saw_fun_pre then
            prerr_endline "funenter without funpre";
          { state' with saw_fun_pre = false }
      | _ ->
          if calls_stacked && state'.saw_fun_pre then
            prerr_endline "funpre without funenter";
          { state' with saw_fun_pre = false }
    in let state' = match op, state.saw_fun_exit with
      | CFunExit { exc = OUndefined }, None ->
          { state' with saw_fun_exit = Value }
      | CFunExit _, None ->
          { state' with saw_fun_exit = Exception }
      | CFunExit { exc = OUndefined }, _ ->
          if calls_normalized then
            prerr_endline "double funexit";
          { state' with saw_fun_exit = Value }
      | CFunExit _, Exception ->
          state' (* Cascading exits due to exception *)
      | CFunExit _, _ ->
          if calls_normalized then
            prerr_endline "double funexit";
          { state' with saw_fun_exit = Exception }
      | CFunPost _, Value ->
          { state' with saw_fun_exit = None }
      | CScriptExc _, Exception ->
          state'
      | CDeclare { declaration_type = CatchParam }, Exception ->
          { state' with saw_fun_exit = None }
      | CThrow _, None ->
          { state' with saw_fun_exit = Exception }
      | CEndExpression, Exception ->
          state' (* Special case: Throw is followed by EndExpression *)
      | _, Exception ->
          prerr_endline "Exception passed through instruction";
          state'
      | op, Value ->
          if calls_stacked then
            Format.eprintf "function return through instruction %a" pp_clean_operation op;
          { state' with saw_fun_exit = None }
      | _, None ->
          state'
    in state'


  let validate level initials trace =
    Log.debug (fun m -> m "Validating trace, level: %s"
      (match level with
           Basic -> "basic"
         | NoUseStrict -> "no use strict"
         | NormalizedCalls -> "calls normalized, removing apply and call"
         | SynthesizedEvents -> "call framing completed"));
    (*
    let function_apply = lookup globals objs ["Function"; "prototype"; "apply"]
    and function_call = lookup globals objs ["Function"; "prototype"; "call"] in 
     *)
      if Debug.is_validate () then begin
        let init = { stack = []; saw_use_strict = false; saw_fun_pre = false; saw_fun_exit = None } in
          S.validation (validate_step level initials.function_apply initials.function_call)
            init trace
      end else
        trace

  let clean_trace ?(up_to = SynthesizeEvents) initials trace =
    match up_to with
        SynthesizeEvents ->
          trace
            |> clean initials
            |> validate Basic initials
            |> remove_use_strict
            |> validate NoUseStrict initials
            |> normalize_calls initials
            |> normalize_function_constructor initials
            |> normalize_eval initials
            |> normalize_string_subscripts
            |> normalize_prototype_manipulation initials
            |> validate NormalizedCalls initials
            |> synthesize_getters_and_setters
            |> synthesize_events initials.functions
            |> validate SynthesizedEvents initials
      | SynthesizeGettersAndSetters ->
          trace
            |> clean initials
            |> validate Basic initials
            |> remove_use_strict
            |> validate NoUseStrict initials
            |> normalize_calls initials
            |> normalize_function_constructor initials
            |> normalize_eval initials
            |> normalize_string_subscripts
            |> normalize_prototype_manipulation initials
            |> validate NormalizedCalls initials
            |> synthesize_getters_and_setters
      | Normalizations ->
          trace
            |> clean initials
            |> validate Basic initials
            |> remove_use_strict
            |> validate NoUseStrict initials
            |> normalize_calls initials
            |> normalize_function_constructor initials
            |> normalize_eval initials
            |> normalize_string_subscripts
            |> normalize_prototype_manipulation initials
      | JustClean ->
          trace
            |> clean initials


  let calculate_clean_trace ?up_to (initials: initials) trace =
    clean_trace ?up_to initials trace
end

module CleanStream = CleanGeneric(StreamTransformers)
module CleanList = CleanGeneric(ListTransformers)

let synthesize_events funcs trace =
  CleanList.synthesize_events funcs trace

let clean_tracefile ?up_to (funs, objs, rawtr, globals, gap, iids) =
  let initials = build_initials objs funs globals gap iids
  in lookup_functions initials;
  (funs, objs, CleanList.clean_trace ?up_to initials rawtr, globals, gap, iids)

let clean_stream ?up_to (data: initials) raw =
  CleanStream.calculate_clean_trace ?up_to data raw

