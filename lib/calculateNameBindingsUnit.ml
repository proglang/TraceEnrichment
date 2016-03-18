open Kaputt.Abbreviations

let (|>) = Pervasives.(|>)

type funid = int
type varid = Local of int | Global of int [@@deriving show, ord, eq]
let pp_funid = Fmt.int

type abstract_event =
  | Call of funid * int * varid list
  | MakeClosure of funid
  | Exit of funid
  | Read of varid
  | Write of varid
  [@@deriving show]
type abstract_trace = (abstract_event * int) list [@@deriving show]

type visible = {
  funcs: varid list BatDynArray.t;
  mutable local_bound: int;
  mutable globals: int
}
type result = {
  trace: abstract_trace;
  num_locals: int;
  num_globals: int;
  visible: varid list list
} [@@deriving show]

let choose_local state visible locals =
  let var = List.nth locals (Random.State.int state (List.length locals))
  in assert (match var with Local var -> var < visible.local_bound | Global _ -> false); var

let generate_fresh_locals state visible locals fid =
  let num = Random.State.int state 10 in
  let fresh =
    let rec seq base n = if n = 0 then [] else Local base :: seq (base+1) (n-1) in
      seq visible.local_bound num
  in visible.local_bound <- visible.local_bound + num;
     (fresh @ BatDynArray.get visible.funcs fid, fresh)


let ctx_src = ref 0

let rec make_abstract_trace state ctx visible locals =
  match Random.State.int state (8 + 5) with
    | 0 ->
        if BatDynArray.length visible.funcs = 0 then
          make_abstract_trace state ctx visible locals
        else begin
          let fid = Random.State.int state (BatDynArray.length visible.funcs) in
          let (locals, fresh) = generate_fresh_locals state visible locals fid in
            incr ctx_src;
            let ctx' = !ctx_src in
            let (trace, distinct, distincts) = make_abstract_trace state ctx' visible locals in
              ((Call (fid, ctx', fresh), ctx) :: trace @ [ Exit fid, ctx ], fresh, distinct :: distincts)
        end
    | 1 ->
        BatDynArray.add visible.funcs locals;
        ([ MakeClosure (BatDynArray.length visible.funcs), ctx ], [], [])
    | 2 -> 
        if locals <> [] then begin
          let var = choose_local state visible locals in
            ([ Read var, ctx ], [var], [])
        end else
            make_abstract_trace state ctx visible locals
    | 3 -> 
        if locals <> [] then begin
          let var = choose_local state visible locals in
            ([ Write var, ctx ], [var], [])
        end else
            make_abstract_trace state ctx visible locals
    | 4 -> 
        if visible.globals > 0 then begin
          let var = Random.State.int state visible.globals in
            ([ Read (Global var), ctx], [Global var], [])
        end else
          make_abstract_trace state ctx visible locals
    | 5 -> 
        if visible.globals > 0 then begin
          let var = Random.State.int state visible.globals in
            ([ Write (Global var), ctx], [Global var], [])
        end else
          make_abstract_trace state ctx visible locals
    | 6 ->
        let id = visible.globals in
          visible.globals <- id + 1;
          ([ Read (Global id), ctx ], [Global id], [])
    | 7 ->
        let id = visible.globals in
          visible.globals <- id + 1;
          ([ Write (Global id), ctx ], [Global id], [])
    | _ ->
        let (trace1, visible1, visibles1) = make_abstract_trace state ctx visible locals
        and (trace2, visible2, visibles2) = make_abstract_trace state ctx visible locals
        in (trace1 @ trace2, visible1 @ visible2, visibles1 @ visibles2)

let check_bounds trace num_locals num_globals =
  let failed = ref false in
  List.iter (function
               | (Call (_, _, vars), _) ->
                   if List.exists (function Local var -> var >= num_locals | Global _ -> true) vars then begin
                     Format.eprintf "Trying to declare local variables %a with only %d locals"
                       (Fmt.list pp_varid) vars num_locals;
                     failed := true
                   end
               | (Read (Local var), _)
               | (Write (Local var), _) ->
                   if var >= num_locals then begin
                     Format.eprintf "Trying to access local variable %d with only %d locals"
                       var num_locals;
                     failed := true
                   end
               | (Read (Global var), _)
               | (Write (Global var), _) ->
                   if var >= num_globals then begin
                     Format.eprintf "Trying to access global variable %d with only %d globals"
                       var num_globals;
                     failed := true
                   end
               | _ -> ())
    trace;
  if !failed then failwith "Errors detected"

let make_abstract_trace state =
  let init = 
      { funcs = BatDynArray.create (); local_bound = 0; globals = 0 } in
  let (trace, visible, visibles) = make_abstract_trace state 0 init [] in
  let () = check_bounds trace init.local_bound init.globals in
    { trace;
      num_locals = init.local_bound;
      num_globals = init.globals;
      visible = visible::visibles }

module Var = struct
  type t = varid
  let compare = compare_varid
  let equal = equal_varid
  let hash = function Local id -> 0 + 2 * id | Global id -> 1 + 2 * id
end
module InterferenceGraph = Graph.Imperative.Graph.Concrete(Var)
module VarMap = BatMap.Make(Var)

let calculate_interference { visible; trace; num_locals; num_globals } =
  let open InterferenceGraph in
  let g = create ~size:(num_locals + num_globals) () in
    (* Add vertices *)
    for i = 0 to num_locals - 1 do add_vertex g (Local i) done;
    for i = 0 to num_globals - 1 do add_vertex g (Global i) done;
    (* Add global interference edges (all globals interfere with each other) *)
    for i = 0 to num_globals - 1 do
      for j = i+1 to num_globals - 1 do
        add_edge g (Global i) (Global j)
      done
    done;
    let all_pairs f l = List.iter (fun x -> List.iter (f x) l) l in
    List.iter (all_pairs (add_edge g)) visible;
      g

module StringSet = BatSet.Make(String)
let generate_string state =
  let len = Random.State.int state 10 + 1 in
  let buf = Buffer.create len in
    for i = 1 to len do
      Buffer.add_char buf (Char.chr ((Char.code 'a') + Random.State.int state 26))
    done;
    Buffer.contents buf

let make_name state interference name_map index =
  if VarMap.mem index name_map then failwith "Calculating name twice";
  let bad_names = InterferenceGraph.fold_succ
                    (fun v bad_names ->
                       match VarMap.Exceptionless.find v name_map with
                         | Some name -> StringSet.add name bad_names
                         | None -> bad_names)
                    interference index StringSet.empty
  and known_names = VarMap.values name_map |> StringSet.of_enum in
  let good_names = StringSet.diff known_names bad_names in
  let name =
    if Random.State.bool state || StringSet.is_empty good_names then begin
      let rec make state =
        let name = generate_string state in
          if StringSet.mem name bad_names then make state else name
      in make state
    end else
      List.nth (StringSet.elements good_names)
        (Random.State.int state (StringSet.cardinal good_names))
  in if StringSet.mem name bad_names then failwith "Tried to generate bad name" else name

let make_names state interference numglobals numlocals =
  let name_map = ref VarMap.empty in
  let add idx =
    name_map := VarMap.add idx (make_name state interference !name_map idx) !name_map
  in
    for i = 0 to numglobals-1 do add (Global i) done;
    for i = 0 to numlocals-1 do add (Local i) done;
    !name_map

let calculate_names state ({ num_globals; num_locals } as data) =
  let inter = calculate_interference data in
    (inter, make_names state inter num_globals num_locals)

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
          | Call (fid, ctx', vars) ->
              CFunPre { f = OFunction(fid, fid); base = OObject 0; args = OObject ctx;
                        call_type = Function } ::
              CFunEnter { f = OFunction(fid, fid); this = OObject 0; args = OObject ctx' } ::
              BatList.map (fun var ->
                             CDeclare { name = VarMap.find var name_map;
                                        declaration_type = Var; value = OUndefined })
                vars
          | MakeClosure fid ->
              [ CLiteral { value = OFunction(fid, fid); hasGetterSetter = false } ]
          | Exit fid  ->
              [ CFunExit { ret = OUndefined; exc = OUndefined };
                CFunPost { f = OFunction(fid, fid); base = OObject 0; args = OObject ctx;
                           result = OUndefined; call_type = Function } ]
          | Read var ->
              begin  try
              [ CRead { name = VarMap.find var name_map; value = OUndefined } ]
              with Not_found -> failwith (Fmt.strf "Tried to map %a, no mapping found" pp_varid var)
              end
          | Write var ->
              begin  try
              [ CWrite { name = VarMap.find var name_map; value = OUndefined;
                         lhs = OUndefined; isSuccessful = true } ]
              with Not_found -> failwith (Fmt.strf "Tried to map %a, no mapping found" pp_varid var)
              end
          in BatList.map (fun op -> (op, facts)) ops)
       trace)

let build_trace state =
  let { trace } as abs = make_abstract_trace state in
  let (inter, names) = calculate_names state abs in
    (abs, concretize trace names, names, inter)

type trace_pair =
    result *
    LocalFacts.arguments_and_closures TraceTypes.enriched_trace *
    string VarMap.t *
    InterferenceGraph.t

module InterferenceGraphDOT = struct
  include InterferenceGraph
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name = function
    | Global i -> "g" ^ string_of_int i
    | Local  i -> "l" ^ string_of_int i
  let vertex_attributes =
    let open Graph.Graphviz.DotAttributes in
    function
    | Global i -> [ `Shape `Box; `Label (string_of_int i) ]
    | Local i -> [ `Shape `Plaintext; `Label (string_of_int i) ]
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end
module InterferenceDOT =
  Graph.Graphviz.Dot(InterferenceGraphDOT)

let output_counter = ref 0
let gen_trace: trace_pair Kaputt.Generator.t = build_trace, fun (abs, conc, map, graph) ->
  let rec make_files num =
    begin
      let base = Fmt.strf "output-%d" !output_counter in
        incr output_counter;
        let report_name = base ^ ".txt"
        and graph_name = base ^ ".dot" in try
          let mode = [ Open_wronly; Open_creat; Open_excl; Open_text ] in
          let report_file = open_out_gen mode 0o644 report_name in
            begin try
              let graph_file = open_out_gen mode 0o644 graph_name in
                (report_name, report_file, graph_name, graph_file)
            with Sys_error e ->
              close_out report_file;
              Sys.remove report_name;
              raise (Sys_error e)
            end
        with Sys_error _ ->
          if (num <= 0) then
            failwith "Can't find an available report file name"
          else
            make_files (num-1)
    end
  in let (report_name, report_file, graph_name, graph_file) =
    make_files 20 in
  let report_fmt = Format.formatter_of_out_channel report_file in
    Fmt.pf report_fmt "@[<v>@[<v 2>Abstract trace:@ %a@]@ " pp_result abs;
    Fmt.pf report_fmt "@[<v 2>Concrete trace:@ %a@]@ "
      (TraceTypes.pp_enriched_trace LocalFacts.pp_arguments_and_closures) conc;
    Fmt.pf report_fmt "@[<v 2>Name map:@ %a@]@ "
      (Fmt.iter_bindings ~sep:Fmt.cut VarMap.iter
         (Fmt.pair ~sep:(Fmt.const Fmt.string " → ") pp_varid Fmt.string))
      map;
    Fmt.pf report_fmt "Interference clusters: See %s@ @]" graph_name;
    Format.pp_print_flush report_fmt ();
    close_out report_file;
    InterferenceDOT.output_graph graph_file graph;
    close_out graph_file;
    report_name

module RM = Reference.ReferenceMap

let check_injective map =
  let seen = VarMap.fold (fun _ ref seen -> RM.modify_def 0 ref (fun x -> x + 1) seen)
               map RM.empty
  in let seen = RM.filterv (fun n -> n > 1) seen
  in if RM.is_empty seen then
    true
  else begin
    Format.printf "Failure: %a" (RM.pp Fmt.int) seen;
    false
  end


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
      (((_, _, names1, _): trace_pair),
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
let collect (_, trace, _, _) = XFRM.collect initials trace
let constant_true _ = true

let tests =
  Test.make_random_test ~title:"Random tests for CalculateNameBindings"
    ~nb_runs:50 gen_trace collect [ constant_true => check_name_maps_injective ]
                                                   
let () = Test.run_test tests
