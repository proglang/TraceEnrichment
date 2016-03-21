open Kaputt.Abbreviations
open Types
open TraceTypes

let (|>) = Pervasives.(|>)

type funid = int
let pp_funid = Fmt.int

let arg_src = ref 0
let fun_src = ref 0

let choose_from_list state list =
  let n = Random.State.int state (List.length list) in
    List.nth list n

let generate_closure state arg closures =
  incr fun_src;
  let fun_id = !fun_src in
  let closures' = IntMap.add fun_id arg closures in
    ([ CLiteral { value = OFunction(fun_id, 0); hasGetterSetter = false },
       (arg, closures') ],
     closures')

let max = 20

let rec make_abstract_trace state bound arg closures =
  match Random.State.int state (if bound < max then 5 else 4) with
    | 0 when IntMap.is_empty closures -> generate_closure state arg closures
    | 1 when IntMap.is_empty closures -> generate_closure state arg closures
    | 2 | 3-> generate_closure state arg closures
    | 0 | 1 ->
        incr arg_src;
        let arg_id = !arg_src
        and (fun_id, _) = choose_from_list state (IntMap.bindings closures) in
        let (trace, closures') =
          make_abstract_trace state (bound - 1) (Some arg_id) closures
        in
          ([ CFunPre { f = OFunction(fun_id, -1); base = OObject 0;
                       args = OObject arg_id; call_type = Function },
             (arg, closures);
             CFunEnter { f = OFunction(fun_id, -1); this = OObject 0;
                         args = OObject arg_id },
             (Some arg_id, closures) ] @
           trace @
           [ CFunExit { ret = ONull; exc = OUndefined },
             (arg, closures');
             CFunPost { f = OFunction(fun_id, -1); base = OObject 0;
                       args = OObject arg_id; call_type = Function;
                       result = ONull },
             (arg, closures') ],
           closures')
    | _ -> 
        let (trace1, closures1) = make_abstract_trace state (bound - 1) arg closures in
        let (trace2, closures2) = make_abstract_trace state (bound - 1) arg closures1 in
          (trace1 @ trace2, closures2)

type abstract_trace = (int option * int option IntMap.t) enriched_trace
let pp_abstract_trace =
  pp_enriched_trace
    (Fmt.pair ~sep:(Fmt.always ";")
       (Fmt.option ~none:(Fmt.always "-") Fmt.int)
       (IntMap.pp (Fmt.option ~none:(Fmt.always "-") Fmt.int)))

let generate_abstract_trace: abstract_trace Kaputt.Generator.t =
  (fun state -> arg_src := 0; fun_src := 0;
                fst (make_abstract_trace state 0 None IntMap.empty)),
  (Fmt.to_to_string pp_abstract_trace)

module Args = LocalFacts.CollectArguments(Streaming.ListTransformers)
module Clos = LocalFacts.CollectClosures(Streaming.ListTransformers)

let collect trace =
  trace |>
    List.map fst |>
    Args.collect |>
    Clos.collect

let num_test = ref 0

let check (abstract, concrete) =
  incr num_test;
  let open LocalFacts in
  let result =
    List.for_all2
      (fun (op, (arg, closures))
             (op', ({ last_arguments = arg';
                      closures = closures' }: arguments_and_closures)) ->
         op = op' &&
         arg = arg' &&
         let closures = IntMap.filter_map (fun _ v -> v) closures in
           IntMap.equal (=) closures closures')
      abstract concrete
  in if result then
    true
  else
    let open Fmt in
      begin
        vbox begin
          prefix (const string "Error detected in test case ") @@
          prefix (const int !num_test) @@
          prefix cut @@
          suffix cut @@
          pair ~sep:cut
            (vbox ~indent:2 begin
               prefix (const string "In:") @@
               prefix cut @@
               pp_abstract_trace
             end)
            (vbox ~indent:2 begin
               prefix (const string "In:") @@
               prefix cut @@
               pp_enriched_trace LocalFacts.pp_arguments_and_closures
             end)
        end
          stdout (abstract, concrete)
      end;
      false


let constant_true _ = true

let tests =
  Test.make_random_test ~title:"Random tests for CalculateNameBindings"
    ~nb_runs:10000 generate_abstract_trace collect [ constant_true => check ]
                                                   
let () = Printexc.record_backtrace true; Test.run_test tests

