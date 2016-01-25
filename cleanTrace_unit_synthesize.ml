open Types
open TraceTypes
open CleanTrace
open Kaputt.Abbreviations

(*val synthesize_events: functions -> clean_trace -> clean_trace*)

let gen_synthesize_inputs max_ht =
  (fun r ->
     let build_string len =
       fst (Kaputt.Generator.string
              (Kaputt.Generator.make_int 1 len)
              Kaputt.Generator.letter) r in
     let build_function_spec r =
       if Random.State.bool r then
         External (Random.State.bits r)
       else
         Local { from_toString = build_string 1024;
                 from_jalangi =
                   if Random.State.bool r then
                     Some (build_string 1024)
                   else
                     None }
     in
     let build_function funcs r =
       let idx = Random.State.int r (ExtArray.length funcs + 1) in
         if idx = ExtArray.length funcs then
           ExtArray.append funcs (build_function_spec r);
         (funcs, OFunction (Random.State.int r 65536, idx))
     and build_jsval r =
       match Random.State.int r 10 with
         | 0 -> OUndefined
         | 1 -> ONull
         | 2 -> OBoolean (Random.State.bool r)
         | 3 -> ONumberInt (Random.State.int r 65536)
         | 4 -> OString (build_string 128)
         | 5 -> OSymbol (build_string 16)
         | 6 -> OObject (Random.State.int r 65536)
         | 7 -> OFunction (Random.State.int r 65536, Random.State.int r 65536)
         | 8 -> OOther (build_string 16, Random.State.int r 65536)
         | _ -> ONumberFloat (Random.State.float r 10.0)
     and build_jsval_defined r =
       match Random.State.int r 9 with
         | 1 -> ONull
         | 2 -> OBoolean (Random.State.bool r)
         | 3 -> ONumberInt (Random.State.int r 65536)
         | 4 -> OString (build_string 128)
         | 5 -> OSymbol (build_string 16)
         | 6 -> OObject (Random.State.int r 65536)
         | 7 -> OFunction (Random.State.int r 65536, Random.State.int r 65536)
         | 8 -> OOther (build_string 16, Random.State.int r 65536)
         | _ -> ONumberFloat (Random.State.float r 10.0)
     and build_call_type r =
       match Random.State.int r 3 with
         | 0 -> Function
         | 1 -> Method
         | _ -> Constructor
     in let build_access r =
       { base = build_jsval_defined r; offset = build_string 32; value = build_jsval r }
     in let build_other r =
       match Random.State.int r 17 with
         | 0 -> CLiteral { value = build_jsval r; hasGetterSetter = Random.State.bool r }
         | 1 -> CForIn (build_jsval r)
         | 2 -> CDeclare { name = build_string 32; value = build_jsval r; declaration_type = Var }
         | 3 -> CDeclare { name = build_string 32; value = build_jsval r; declaration_type = ArgumentArray }
         | 4 -> CDeclare { name = build_string 32; value = build_jsval r; declaration_type = ArgumentBinding (Random.State.int r 32)  }
         | 5 -> CGetFieldPre (build_jsval r, build_string 32)
         | 6 -> CPutFieldPre (build_access r)
         | 7 -> CGetField (build_access r)
         | 8 -> CPutField (build_access r)
         | 9 -> CRead { name = build_string 32; value = build_jsval r; isGlobal = Random.State.bool r }
         | 10 -> CWrite { name = build_string 32; value = build_jsval r; isGlobal = Random.State.bool r;
                          isSuccessful = Random.State.bool r; lhs = build_jsval r }
         | 11 -> CReturn (build_jsval r)
         | 12 -> CWith (build_jsval r)
         | 13 -> CBinary { op = build_string 3; left = build_jsval r; right = build_jsval r; result = build_jsval r }
         | 14 -> CUnary { op = build_string 3; arg = build_jsval r; result = build_jsval r }
         | 15 -> CConditional (build_jsval r)
         | _ -> CEndExpression

     in
     let rec build_trace ht funcs =
       match if ht < max_ht then Random.State.int r 6 else 2 + Random.State.int r 4 with
         | 0 ->
             let (funcs, f) = build_function funcs r
             and this = build_jsval r
             and args = build_jsval r
             and args' = build_jsval r
             and call_type = build_call_type r
             and result = build_jsval r in
             let (funcs, t) = build_trace (ht + 1) funcs
             in (funcs,
                 [ CFunPre { f; base=this; args; call_type };
                   CFunEnter { f; this; args = args' } ] @
                 t @
                 [ CFunExit { ret = result; exc = OUndefined };
                   CFunPost { f; base=this; args; call_type; result } ])
         | 1 ->
             let (funcs, t) = build_trace (ht + 1) funcs
             in (funcs, [ CScriptEnter ] @ t @ [ CScriptExit ])
         | 2 ->
             let exc = build_jsval_defined r in
             let (funcs, t) = build_unwind (ht + 1) funcs exc
             in (funcs, t @ [ CDeclare { name = ""; value = exc; declaration_type = CatchParam } ])
         | 3 ->
             (funcs, [])
         | 4 ->
             let (funcs, t1) = build_trace ht funcs in
             let (funcs, t2) = build_trace ht funcs in
               (funcs, t1 @ t2)
         | _ ->
             (funcs, [build_other r])
     and build_unwind ht funcs exc =
       match if ht < max_ht then Random.State.int r 4 else 2 + Random.State.int r 2 with
         | 0 ->
             let (funcs, f) = build_function funcs r
             and this = build_jsval r
             and args = build_jsval r
             and args' = build_jsval r
             and call_type = build_call_type r in
             let (funcs, u) = build_unwind (ht + 1) funcs exc
             in (funcs,
                 [ CFunPre { f; base=this; args; call_type };
                   CFunEnter { f; this; args = args' } ] @
                 u @
                 [ CFunExit { exc; ret = OUndefined } ])
         | 1 ->
             let (funcs, u) = build_unwind (ht + 1) funcs exc in
               (funcs, [ CScriptEnter ] @ u @ [ CScriptExc exc ])
         | 2 ->
             (funcs, [ CThrow exc ])
         | _ ->
             let (funcs, t) = build_trace ht funcs in
             let (funcs, u) = build_unwind ht funcs exc in
               (funcs, t @ u)
     in Printexc.print (build_trace 0) (ExtArray.of_list [])),
  (fun x -> Misc.to_string (FormatHelper.pp_print_pair pp_functions pp_clean_trace) x ^ "\n")

let is_instrumented funcs f =
  match ExtArray.get funcs f with
    | Local { from_jalangi = Some _ } -> true
    | _ -> false

let drop funcs trace =
  let rec drop stack trace = match trace with
    | [] -> []
    | event :: trace ->
        let trace'= match event with
          | CFunPre { f = OFunction (_, f) } ->
              drop (is_instrumented funcs f :: stack) trace
          | CFunExit _ ->
              drop (List.tl stack) trace
          | _ ->
              drop stack trace
        in match stack with
          | true :: _ | [] -> event :: trace'
          | false :: _ -> trace'
  in drop [] trace

type stack_entry =
  | Func of funpre
  | Script

let valid trace =
  let rec check_no_exc stack = function
    | [] -> stack = []
    | CFunPre funpre :: CFunEnter funenter :: trace ->
        funpre.f = funenter.f &&
        funpre.base = funenter.this &&
        check_no_exc (Func funpre :: stack) trace
    | CFunExit { exc = OUndefined; ret } :: CFunPost funpost :: trace ->
        begin
          match stack with
            | Func funpre :: stack ->
                funpre.f = funpost.f &&
                funpre.base = funpost.base &&
                funpre.args = funpost.args &&
                funpre.call_type = funpost.call_type &&
                funpost.result = ret &&
                check_no_exc stack trace
            | _ -> false
        end
    | CScriptEnter :: trace ->
        check_no_exc (Script :: stack) trace
    | CScriptExit :: trace ->
        begin
          match stack with
            | Script :: stack ->
                check_no_exc stack trace
            | _ -> false
        end
    | CLiteral _ :: trace
    | CForIn _ :: trace
    | CGetFieldPre _ :: trace
    | CPutFieldPre _ :: trace
    | CGetField _ :: trace
    | CPutField _ :: trace
    | CRead _ :: trace
    | CWrite _ :: trace
    | CReturn _ :: trace
    | CWith _ :: trace
    | CBinary _ :: trace
    | CUnary _ :: trace
    | CEndExpression :: trace
    | CConditional _ :: trace -> check_no_exc stack trace
    | CThrow exc :: trace -> check_exc stack exc trace
    | CDeclare { declaration_type = CatchParam } :: _ -> false
    | CDeclare _ :: trace -> check_no_exc stack trace
    | _ -> false
   and check_exc stack exc = function
     | CFunExit { exc=exc'; ret = OUndefined } :: trace ->
         begin
           match stack with
             | Func _ :: stack ->
                 exc = exc' && check_exc stack exc trace
             | _ -> false
         end
     | CScriptExc exc' :: trace ->
         begin
           match stack with
             | Script :: stack ->
                 exc = exc' && check_exc stack exc trace
             | _ -> false
         end
     | CDeclare { declaration_type = CatchParam; value } :: trace ->
         exc = value && check_no_exc stack trace
     | _ -> false
  in check_no_exc [] trace


let check ((funcs, trace), trace') =
  let dtrace = drop funcs trace
  and dtrace' = drop funcs trace'
  in if dtrace <> dtrace' then begin
    Format.eprintf "Mismatch between dropped traces: @[<v 2>@ %a@ @] vs. @[<v 2>@ %a@]@."
      pp_clean_trace dtrace pp_clean_trace dtrace';
    false
  end else if not (valid trace') then begin
    Format.eprintf "@[<v>Not a valid trace: @[<v 2>@ %a@]@ Generated from @[<v 2>@ %a@]@ by@[<v 2>@ %a@]@."
      pp_clean_trace trace' pp_clean_trace trace pp_clean_trace dtrace;
    false
  end else
    true

let test_synthesize_events =
  Test.make_random_test
    (gen_synthesize_inputs 40)
    (fun (funcs, trace) -> Printexc.print (synthesize_events funcs) (Printexc.print (drop funcs) trace))
    [ Kaputt.Specification.always => Printexc.print check ]
    ~nb_runs:1000000 ~title:"Specification-based test for synthesize_events"

let _ =
  Printexc.record_backtrace true;
  Test.run_tests ( [ test_synthesize_events ] )
