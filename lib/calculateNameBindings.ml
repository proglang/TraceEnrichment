open Reference
open Types
open Streaming
open TraceTypes
open LocalFacts

type name_map = reference StringMap.t
type state = {
  local_names: name_map list;
  global_names: name_map;
  closures: Reference.reference StringMap.t IntMap.t;
  closure_name: int list;
}

let bind name ref state =
  match state.local_names with
    | [] -> failwith "Empty local name stack"
    | local_names :: local_names' ->
        let local_names = StringMap.add name ref local_names :: local_names'
        in match ref with
          | Variable (Global, _) ->
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
  in let env = StringMap.add "this" (Variable (Local fid, "this")) env
  in { state with local_names = env :: state.local_names;
                  closure_name = fid :: state.closure_name }

let make_exit { local_names; global_names; closures; closure_name } =
  match closure_name, local_names with
    | closure :: closure_name, local :: local_names ->
        { local_names; global_names; closures = IntMap.add closure local closures;
          closure_name }
    | _, _ -> failwith "Empty closure or local stack"

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
    | _ ->
      state in
  ( (op, { last_arguments = facts.last_arguments;
           closures = state.closures;
           names = merge state.global_names (List.hd state.local_names) }),
    res )

let initial_vars globals_are_properties =
  let global_names =
    if globals_are_properties then
      StringMap.add "this" (Variable (Global, "this")) StringMap.empty
    else
      failwith "Initial variable calculation not supported for non-property globals"
  in { global_names; local_names = [ StringMap.empty ]; closures = IntMap.empty;
       closure_name = [] }

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
      (initial_vars globals_are_properties)
      (fun state (op, facts) ->
         collect_names_step objects globals_are_properties state facts op)
      tr
end

