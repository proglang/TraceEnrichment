open Kaputt.Abbreviations

let (|>) = Pervasives.(|>)

type funid = int
type varid = int
let pp_varid = Fmt.int
let pp_funid = Fmt.int

type abstract_event =
  | Call of funid * varid list
  | MakeClosure of funid
  | Exit of funid
  | Read of varid
  | Write of varid
  | ReadGlobal of varid
  | WriteGlobal of varid
  [@@deriving show]
type abstract_trace = (abstract_event * int) list [@@deriving show]

type visible = {
  funcs: varid list BatDynArray.t;
  locals: varid list;
  mutable local_bound: int;
  mutable globals: varid
}

let choose_local state { locals } =
  List.nth locals (Random.State.int state (List.length locals))

let generate_fresh_locals state visible fid =
  let num = Random.State.int state 10 in
  let fresh =
    let rec seq base n = if n = 0 then [] else base :: seq (base+1) (n-1) in
      seq visible.local_bound num
  in visible.local_bound <- visible.local_bound + num;
     ({ visible with locals = fresh @ BatDynArray.get visible.funcs fid }, fresh)


let ctx_src = ref 0

let rec make_abstract_trace state ctx visible =
  match Random.State.int state (8 + 3) with
    | 0 ->
        let fid = Random.State.int state (BatDynArray.length visible.funcs) in
        let (visible, fresh) = generate_fresh_locals state visible fid in
          incr ctx_src;
        let ctx' = !ctx_src in
        let (trace, distinct, distincts) = make_abstract_trace state ctx' visible in
          ((Call (fid, fresh), ctx) :: trace @ [ Exit fid, ctx ], [], distinct :: distincts)
    | 1 ->
        BatDynArray.add visible.funcs visible.locals;
        ([ MakeClosure (BatDynArray.length visible.funcs), ctx ], [], [])
    | 2 -> 
        let var = choose_local state visible in ([ Read var, ctx ], [var, true], [])
    | 3 -> 
        let var = choose_local state visible in ([ Write var, ctx ], [var, true], [])
    | 4 -> 
        let var = Random.State.int state visible.globals in
          ([ ReadGlobal var, ctx], [var, false], [])
    | 5 -> 
        let var = Random.State.int state visible.globals in
          ([ WriteGlobal var, ctx], [var, false], [])
    | 6 ->
        let id = visible.globals in
          visible.globals <- id + 1;
          ([ ReadGlobal id, ctx ], [id, false], [])
    | 7 ->
        let id = visible.globals in
          visible.globals <- id + 1;
          ([ WriteGlobal id, ctx ], [id, false], [])
    | _ ->
        let (trace1, visible1, visibles1) = make_abstract_trace state ctx visible
        and (trace2, visible2, visibles2) = make_abstract_trace state ctx visible
        in (trace1 @ trace2, visible1 @ visible2, visibles1 @ visibles2)

let make_abstract_trace state =
  let init = 
      { funcs = BatDynArray.create (); locals = []; local_bound = 0; globals = 0 } in
  let (trace, visible, visibles) = make_abstract_trace state 0 init in
    (trace, visible :: visibles, init.local_bound, init.globals)

module Var = struct type t = int * bool [@@deriving ord] end
module VarSet = BatSet.Make(Var)
module VarMap = BatMap.Make(Var)

let add_interference_impl v1 v2 i =
  VarMap.modify_def VarSet.empty v1 (VarSet.add v2) i

let add_interference v1 v2 i =
  add_interference_impl v1 v2 i |> add_interference_impl v2 v1

let interferes v1 v2 i =
  match VarMap.Exceptionless.find v1 i with
    | Some s -> VarMap.mem v2 i
    | None -> false

let rec fold_pairs f a = function
  | [] -> a
  | x::l -> List.fold_left (fun a y -> f x y a) a l

let calculate_interference visibles =
  List.fold_left (fold_pairs add_interference) VarMap.empty visibles

let make_name state interference name_map index =
  let interference_set =
    match VarMap.Exceptionless.find index interference with
      | Some s -> s
      | None -> VarSet.empty in
  let interferes name =
    VarSet.exists (fun v -> VarMap.Exceptionless.find v name_map = Some name)
      interference_set
  in

  let generate_string state =
    let len = Random.State.int state 10 + 1 in
    let buf = Buffer.create len in
      for i = 1 to len do
        Buffer.add_char buf (Char.chr ((Char.code 'a') + Random.State.int state 26))
      done;
      Buffer.contents buf

  in let names = VarMap.fold (fun _ name names -> name :: names) name_map []

  in let rec make state =
    let name =
      if Random.State.bool state then
        generate_string state
      else
        List.nth names (Random.State.int state (List.length names))
    in if interferes name then make state else name

  in make state

let make_names state interference numglobals numlocals =
  let name_map = ref VarMap.empty in
  let add idx =
    name_map := VarMap.add idx (make_name state interference !name_map idx) !name_map
  in
    for i = 0 to numglobals do add (i, false) done;
    for i = 0 to numlocals do add (i, true) done;
    !name_map

let calculate_names state (_, visible, numglobals, numlocals) =
  make_names state (calculate_interference visible) numglobals numlocals

let concretize trace name_map =
  let open TraceTypes in
  let open LocalFacts in
  let open Types in
  let closure_map =
    List.fold_left (fun closure_map (op, ctx) ->
                      match op with
                        | MakeClosure funid ->
                            IntMap.add funid ctx closure_map
                        | _ -> closure_map)
      IntMap.empty trace
  in
  BatList.flatten
    (BatList.map
       (fun (op, ctx) ->
          let facts =
            { last_arguments = if ctx = 0 then None else Some ctx; closures = closure_map }
          in let ops = match op with
          | Call (fid, vars) ->
              CFunPre { f = OFunction(fid, fid); base = OObject 0; args = OObject ctx;
                        call_type = Function } ::
              CFunEnter { f = OFunction(fid, fid); this = OObject 0; args = OObject ctx } ::
              BatList.map (fun var ->
                             CDeclare { name = VarMap.find (var, false) name_map;
                                        declaration_type = Var; value = OUndefined })
                vars
          | MakeClosure fid ->
              [ CLiteral { value = OFunction(fid, fid); hasGetterSetter = false } ]
          | Exit fid  ->
              [ CFunExit { ret = OUndefined; exc = OUndefined };
                CFunPost { f = OFunction(fid, fid); base = OObject 0; args = OObject ctx;
                           result = OUndefined; call_type = Function } ]
          | Read var ->
              [ CRead { name = VarMap.find (var, true) name_map; value = OUndefined } ]
          | Write var ->
              [ CWrite { name = VarMap.find (var, true) name_map; value = OUndefined;
                         lhs = OUndefined; isSuccessful = true } ]
          | ReadGlobal var ->
              [ CRead { name = VarMap.find (var, false) name_map; value = OUndefined } ]
          | WriteGlobal var ->
              [ CWrite { name = VarMap.find (var, false) name_map; value = OUndefined;
                         lhs = OUndefined; isSuccessful = true } ]
          in BatList.map (fun op -> (op, facts)) ops)
       trace)

let build_trace state =
  let (trace, _, _, _) as abs = make_abstract_trace state in
  let names = calculate_names state abs in
    (trace, concretize trace names, names)

type trace_pair =
    abstract_trace *
    LocalFacts.arguments_and_closures TraceTypes.enriched_trace *
    string VarMap.t

let gen_trace: trace_pair Kaputt.Generator.t = build_trace, fun (abs, conc, map) ->
  Fmt.strf "@[<v>@[<v 2>Abstract:@ %a@]@ @[<v 2>Concrete:@ %a@]@ @[<v 2>Name map:@ %a@]@]"
    pp_abstract_trace abs
    (TraceTypes.pp_enriched_trace LocalFacts.pp_arguments_and_closures) conc
    (Fmt.iter_bindings ~sep:Fmt.cut VarMap.iter
       (fun pp ((id, local), name) ->
          Fmt.pf pp "%d%s -> %s" id (if local then "" else ":global") name))
    map

module RM = Reference.ReferenceMap

let check_injective map =
  let seen = VarMap.fold (fun _ ref seen -> RM.modify_def 0 ref (fun x -> x + 1) seen)
               map RM.empty
  in let seen = RM.filterv (fun n -> n > 1) seen
  in RM.is_empty seen

let map_compose names1 names2 =
  VarMap.fold (fun key name names ->
                 match StringMap.Exceptionless.find name names2 with
                   | Some name' -> VarMap.add key name' names
                   | None -> names)
    names1 VarMap.empty

let check_composed_injective names1 names2 =
  check_injective (map_compose names1 names2)

let check_name_map_injective names1 (_, (facts: LocalFacts.names_resolved)) =
  check_composed_injective names1 facts.LocalFacts.names

let check_name_maps_injective
      (((_, _, names1): trace_pair),
       (trace: LocalFacts.names_resolved TraceTypes.enriched_trace)) =
  List.for_all (check_name_map_injective names1) trace

let initials =
  let open Types in
  let objects = BatDynArray.create() in
    BatDynArray.add objects StringMap.empty;
    { objects; globals = StringMap.empty; globals_are_properties = true;
      functions = BatDynArray.create();
      function_apply = OUndefined;
      function_call = OUndefined;
      function_constructor = OUndefined;
      function_eval = OUndefined;
    }

module XFRM = CalculateNameBindings.Make(Streaming.ListTransformers)
let collect (_, trace, _) = XFRM.collect initials trace
let constant_true _ = true

let tests =
  Test.make_random_test ~title:"Random tests for CalculateNameBindings"
    ~nb_runs:20 gen_trace collect [ constant_true => check_name_maps_injective ]
                                                   
let () = Test.run_test tests
