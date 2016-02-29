open Reference
open Types
open Streaming
open TraceTypes
open LocalFacts

let update_top newtop facts = newtop :: List.tl facts

let make_declare globals_are_properties decltype objects last_arguments
      facts name =
  let new_top = match decltype with
    | ArgumentBinding i ->
        begin match last_arguments with
          | Some arg ->
              let ref =
                if StringMap.mem (string_of_int i) (BatDynArray.get objects arg) then
                  Reference.reference_of_field (OObject arg) (string_of_int i)
                else
                  Reference.reference_of_local_name arg name
              in
                StringMap.add name ref (List.hd facts)
          | None ->
              failwith "Argument binding outside a function"
        end
    | _ ->
        (* FIXME this doesn't scope catches correctly. Solve this in another iteration. *)
        let ref = begin match last_arguments with
          | Some arg ->
              Reference.reference_of_local_name arg name
          | None ->
              Reference.reference_of_name globals_are_properties
                (StringMap.remove name (List.hd facts)) name
        end in StringMap.add name ref (List.hd facts)
  in update_top new_top facts

let make_global_if_new globals_are_properties name facts =
  if StringMap.mem name (List.hd facts) then
    facts
  else
    let new_top =
      StringMap.add name
        (Reference.reference_of_name globals_are_properties (List.hd facts) name)
        (List.hd facts)
    in update_top new_top facts

let make_enter f args state facts =
  raise Exit

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
        make_enter f args state facts
    | CFunPost _ ->
        List.tl state
    | _ ->
      state in
  ( (op, { last_arguments = facts.last_arguments;
           closures = facts.closures;
           names = List.hd res }),
    res )

let initial_vars globals_are_properties =
  if globals_are_properties then
    StringMap.add "this" (Variable (Global, "this")) StringMap.empty
  else
    failwith "Initial variable calculation not supported for non-property globals"
  
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
      [initial_vars globals_are_properties]
      (fun state (op, facts) ->
         collect_names_step objects globals_are_properties state facts op)
      tr
end

