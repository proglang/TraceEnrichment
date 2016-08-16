open Reference
open TypesJS
open Streaming
open TraceTypes

type version_state = {
  current_version: int ReferenceMap.t;
  last_update: versioned_reference option;
  fresh: versioned_reference list
}
let pp_version_state pp { current_version; last_update; fresh } =
  Format.fprintf pp "@[<v 2>last_update = %a, fresh = %a, current versions:@ %a@]"
    (Fmt.option Reference.pp_versioned_reference) last_update
    (Fmt.list Reference.pp_versioned_reference) fresh
    (ReferenceMap.pp ~entry_sep:Fmt.cut ~pair_sep:(Fmt.always ": ") Fmt.int) current_version

let increment_reference state ref =
  match ReferenceMap.Exceptionless.find ref state.current_version with
    | Some v ->
        { current_version = ReferenceMap.add ref (v + 1) state.current_version;
          last_update = Some (ref, v);
          fresh = (ref, v) :: state.fresh
        }
    | None ->
        { current_version = ReferenceMap.add ref 0 state.current_version;
          last_update = Some (ref, 0);
          fresh = (ref, 0) :: state.fresh
        }

let warnings: string list ref = ref []

let provide_read ref state =
  if ReferenceMap.mem ref state.current_version then
    state
  else
    increment_reference state ref

let provide_write (objects: objects) ref state =
  if ReferenceMap.mem ref state.current_version then
    match ref with
    | Field (obj, fld) ->
      (* If the field is not writable, do nothing. *)
      begin try
          let objid = get_object_id obj in
          let fldspec = StringMap.find fld (BatDynArray.get objects objid) in
          if fldspec.writable && fldspec.set = None then
            increment_reference state ref
          else if fldspec.set = None then
            state
          else begin
            (* Set handlers can do whatever they like. Assume the
             * update goes through, but warn about possible
             * unsoundness. *)
            let msg =
              Format.sprintf "Writing to %a@%s with set handler"
                (fun () -> Fmt.to_to_string pp_objectid) obj fld
            in warnings := msg :: !warnings;
            increment_reference state ref
          end
        with Not_found ->
          (* A new field. TODO: Can objects prevent this from happening? *)
          increment_reference state ref
      end
    | Variable (Global, name) ->
      (* Apparently, global variables may be read-only (e.g., console in
       * node.js. Since we cannot detect this as of now, just assume
       * it goes through and warn about possible unsoundness. *)
      let msg = Format.sprintf "Writing to global variable %s" name in
      warnings := msg :: !warnings;
      increment_reference state ref
    | Variable (Local _, _) ->
      increment_reference state ref
  else
    increment_reference state ref

let provide_object (objects: objects) state obj =
  (* Recurse over object structure, initialize all fields that have not
   * been seen yet. *)
  let rec recurse obj state =
    StringMap.fold
      (fun name (field: fieldspec) state ->
         let ref = reference_of_fieldref (obj, name) in
         if ReferenceMap.mem ref state.current_version then
           state
         else
           increment_reference state ref |> recurse_value field.value)
      (BatDynArray.get objects (get_object_id obj)) state
  and recurse_value field state = match field with
    | OObject _ | OOther _ | OFunction _ -> recurse (objectid_of_jsval field) state
    | _ -> state
  in recurse obj state

let provide_argument_alias objects state name arguments i =
  let field = string_of_int i in
  match arguments with
  | Some params when StringMap.mem field (BatDynArray.get objects params) ->
      state (* It's an alias, no need for updating *)
  | Some arguments ->
    (* Argh. Javascript.
     * arguments reflects the *actual* parameters, while name bindings reflect
     * the *formal* parameters. Of course,  if there are less actual then formal
     * parameters, we cannot possibly name-bind some field in the arguments
     * object, can we? *)
    provide_write objects (reference_of_local_name arguments name) state
  | None -> failwith "No arguments to alias!"

let provide_literal (objs: objects) state = function
  | (OFunction _ | OOther _ | OObject _) as o ->
    provide_object objs state (objectid_of_jsval o)
  | _ -> state

let collect_versions_step (objects: objects) globals_are_properties state
      (facts: LocalFacts.prototypes_resolved) op =
  let open LocalFacts in
  let state = { state with fresh = [] } in
  let nameref =
    reference_of_name globals_are_properties facts.names in
  let declare_var name =
      provide_write objects (reference_of_name globals_are_properties facts.names name) in
  let res = match op with
    | CFunPre { base; args } ->
        let state = provide_literal objects state args
        in provide_literal objects state base
    | CLiteral { value } ->
      provide_literal objects state value
    | CDeclare { name; declaration_type = ArgumentBinding i } ->
      provide_argument_alias objects state name facts.last_arguments i
    | CDeclare { name } ->
        declare_var name state
    | CGetField { actual_base; offset } ->
      provide_read (reference_of_field actual_base offset) state
    | CPutField { actual_base; offset } ->
      provide_write objects (reference_of_field actual_base offset) state
    | CRead { name } ->
      provide_read (nameref name) state
    | CWrite { name } ->
      provide_write objects (nameref name) state
    | CFunEnter { this; args } ->
        let state =  provide_literal objects state args in
        let state = provide_literal objects state this in
          declare_var "this" state
    | _ ->
      state in
  Logs.debug
    (fun fmt ->
       fmt "@[<v 2>Collecting versions for %a where %a.@ Old state: %a@ New state: %a@]"
         pp_clean_operation op
         pp_prototypes_resolved facts
         pp_version_state state
         pp_version_state res);
  ( (op, { last_arguments = facts.last_arguments;
           closures = facts.closures;
           last_update = res.last_update;
           versions = res.current_version;
           names = facts.names;
           prototypes = facts.prototypes;
           fresh_versioned_references = res.fresh }),
    res )

let initial_refs objects globals_are_properties globals =
  let reference_of_global =
    reference_of_name globals_are_properties StringMap.empty
  in StringMap.fold
    (fun var id refs ->
       let refs =
         if var = "global" then
           refs
         else
           provide_read (reference_of_global var) refs
       in provide_object objects refs (objectid_of_jsval id))
    globals
    {
      current_version = ReferenceMap.empty;
      last_update = None;
      fresh = []
    }

let initial_versions objs globals gap  =
  (initial_refs objs gap globals).current_version

module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_operation * LocalFacts.prototypes_resolved) trace ->
    (clean_operation * LocalFacts.versions_resolved) trace
end
module Make (T: Transformers) = struct
  type 'a trace = 'a T.sequence
  let collect {objects; globals_are_properties; globals} tr =
    T.map_state
      (initial_refs objects globals_are_properties globals)
      (fun state (op, facts) ->
         collect_versions_step objects globals_are_properties state facts op)
      tr
end

