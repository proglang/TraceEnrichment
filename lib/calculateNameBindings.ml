open Reference
open Types
open Streaming
open TraceTypes
open LocalFacts

type name_map = reference StringMap.t
let pp_name_map = StringMap.pp pp_reference
type state = {
  local_names: name_map list;
  global_names: name_map;
  closures: Reference.reference StringMap.t IntMap.t;
  old_arguments: int option;
}
let pp_state pp { local_names; global_names; closures; old_arguments } =
  Format.fprintf pp
    "@[<v>local_names = @[<v>%a@];@ global_names = @[<hov>%a@];@ closures = @[<v>%a@]@ old_arguments = %a"
    (Fmt.list (Fmt.box ~indent:2 pp_name_map)) local_names
    pp_name_map global_names
    (IntMap.pp ~pair_sep:(Fmt.always ":") ~entry_sep:Fmt.cut ~entry_frame:(Fmt.box ~indent:2)
                            (StringMap.pp ~pair_sep:(Fmt.always " -> ") Reference.pp_reference))
    closures
    (Fmt.option Fmt.int) old_arguments


let bind name ref state =
  match state.local_names with
    | [] -> failwith "Empty local name stack"
    | local_names :: local_names' ->
        let local_names = StringMap.add name ref local_names :: local_names'
        in match ref with
          | Variable (Global, _)
          | Field (Object 0, _) ->
              { state with local_names;
                           global_names = StringMap.add name ref state.global_names }
          | _ -> { state with local_names }

let make_global globals_are_properties name =
  if globals_are_properties then Field (Object 0, name) else Variable (Global, name)

let make_declare globals_are_properties decltype objects last_arguments
      facts name =
  let ref = match decltype, last_arguments with
    | ArgumentBinding i, Some arg when
        StringMap.mem (string_of_int i) (BatDynArray.get objects arg)->
        Field (Object arg, string_of_int i)
    | ArgumentBinding _, Some arg ->
        Variable (Local arg, name)
    | ArgumentBinding _, None ->
        failwith "Argument binding outside a function"
    | _, Some arg ->
        Variable (Local arg, name)
    | _, None ->
        (* FIXME This is not quite correct for catch parameters.
         * But since we don't have proper scoping yet, we can't fix it. *)
        make_global globals_are_properties name
  in bind name ref facts

let get_environment { local_names } = List.hd local_names

let make_global_if_new globals_are_properties name facts =
  if StringMap.mem name (get_environment facts) ||
     StringMap.mem name facts.global_names then
    facts
  else
    bind name (make_global globals_are_properties name) facts

let make_enter f args (facts: arguments_and_closures) state =
  let fid = match f with
    | OFunction (fid, _) -> fid
    | _ -> failwith "Calling something that is not a function"
  in let env =
    match IntMap.Exceptionless.find fid facts.closures with
      | Some closure ->
          begin try
            IntMap.find closure state.closures
          with Not_found ->
            failwith ("Closure environment for " ^ string_of_int fid ^ " not found")
          end
      | None -> StringMap.empty
  in let env = StringMap.add "this" (Variable (Local (BatOption.get facts.last_arguments), "this")) env
  in { state with local_names = env :: state.local_names; old_arguments = facts.last_arguments }

let make_exit { local_names; global_names; closures; old_arguments } =
  match local_names with
    | local :: local_names ->
        let arg = BatOption.get old_arguments in
        { local_names; global_names; old_arguments;
          closures = IntMap.add arg local closures }
    | [] -> failwith "Empty local stack"

let make_call { local_names; global_names; closures; old_arguments } =
  match local_names, old_arguments with
    | local :: _, Some arg ->
        { local_names; global_names; old_arguments;
          closures = IntMap.add arg local closures }
    | _ -> { local_names; global_names; closures; old_arguments }

let merge =
  StringMap.merge (fun _ global local ->
                     match local with None -> global | Some _ -> local)

let collect_names_step (objects: objects) globals_are_properties state
      (facts: LocalFacts.arguments_and_closures) op =
  let open LocalFacts in
  let res = match op with
    | CDeclare { name; declaration_type } ->
        make_declare globals_are_properties declaration_type objects
          facts.last_arguments state name
    | CRead { name } ->
        make_global_if_new globals_are_properties name state
    | CWrite { name } ->
        make_global_if_new globals_are_properties name state
    | CFunEnter { f; args } ->
        make_enter f args facts state
    | CFunExit _ ->
        make_exit state
    | CFunPre _ ->
        make_call state
    | _ ->
      state in
    (*
    Logs.debug (fun fmt ->
                  fmt "@[<v 2>Collecting names for %a@ Arguments: %a@ Closures: [%a]@ @[<v 2>Old state:@ %a@]@ @[<v 2>New state:@ %a@]@ @]"
                    pp_clean_operation op
                    (Fmt.option Fmt.int) facts.last_arguments
                    (IntMap.pp Fmt.int) facts.closures
                    pp_state state
                    pp_state res);
     *)
  ( (op, { last_arguments = facts.last_arguments;
           closures = res.closures;
           names = merge res.global_names (List.hd res.local_names) }),
    res )

let initial_vars objects globals_are_properties =
  let global_names =
    if not globals_are_properties then
      failwith "Initial variable calculation not supported for non-property globals";
    let init_names = StringMap.mapi
                       (fun name _ -> Field (Object 0, name))
                       (BatDynArray.get objects 0)
    in StringMap.add "this" (Variable (Global, "this")) init_names
  in { global_names; local_names = [ StringMap.empty ]; closures = IntMap.empty; old_arguments = None }

module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_operation * LocalFacts.arguments_and_closures) trace ->
    (clean_operation * LocalFacts.names_resolved) trace
end
module Make (T: Transformers) = struct
  type 'a trace = 'a T.sequence
  let collect {objects; globals_are_properties; globals} tr =
    T.map_state
      (initial_vars objects globals_are_properties)
      (fun state (op, facts) ->
         collect_names_step objects globals_are_properties state facts op)
      tr
end

