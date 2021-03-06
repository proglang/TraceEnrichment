open TypesJS
open TraceTypes
open LocalFacts

module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_event * LocalFacts.versions_resolved) trace ->
    (clean_event * LocalFacts.local_facts) trace
end
module VersionedReferenceMap = Reference.VersionedReferenceMap
module ReferenceMap = Reference.ReferenceMap
type points_to_map = Reference.points_to_map

let make_versioned ({ versions }: LocalFacts.versions_resolved) ref =
  LocalFacts.make_versioned_impl versions ref

let add_write (facts: LocalFacts.versions_resolved)
      state ref value may_be_known: points_to_map =
  let vref = make_versioned facts ref in
    if VersionedReferenceMap.mem state vref then begin
      (* This write was dropped; most likely, the field was marked
       * "not writable". *)
      if not may_be_known then
        Format.eprintf
          "Weirdness detected: Write of %a failed (ref: %a)@."
          Reference.pp_reference ref
          Reference.pp_versioned_reference vref;
      state
    end else
      VersionedReferenceMap.add state vref value

let add_read (facts: LocalFacts.versions_resolved) state ref value: points_to_map =
  let vref = make_versioned facts ref in
  if VersionedReferenceMap.mem state vref then begin
    if (value <> VersionedReferenceMap.find state vref) then begin
      Format.eprintf
        "Weirdness detected: In read of %a, expected %a, but got %a@."
        Reference.pp_reference ref
        pp_jsval (VersionedReferenceMap.find state vref)
        pp_jsval value
    end;
    state
  end else
    VersionedReferenceMap.add state vref value

let add_known_new_object objects (facts: LocalFacts.versions_resolved) state obj =
  let id = objectid_of_jsval obj in
  let may_be_known = (Some (get_object_id id) = facts.last_arguments) in
  StringMap.fold
    (fun name (objspec: fieldspec) state ->
       add_write facts state
         (Reference.reference_of_fieldref (id, name))
         objspec.value may_be_known)
    (BatDynArray.get objects(get_object_id id))
    state

(* This is terribly inefficient! *)
    (*
let add_literal objects (facts: versions_resolved) state value =
  (* HACK use the fact that all references in state.versions should be *)
  (* mapped to find missing fields.  *)
  ReferenceMap.fold
    (fun ref ver state ->
       let vref = (ref, ver) in
       if VersionedReferenceMap.mem state vref then
         state
       else
         match ref with
         | Reference.Field (obj, field) ->
           let objid = get_object_id obj in
           let ({ value }: fieldspec) =
             StringMap.find field (BatDynArray.get objects objid)
           in
           VersionedReferenceMap.add state vref value
         | Reference.Variable (_, name) ->
             failwith ("Unexpected unmapped variable " ^ name))
    facts.versions
    state
     *)

let add_literal (objects: objects) ({ versions }: versions_resolved) state value =
  let rec add_props state objid =
    StringMap.fold (fun prop ({ value }: fieldspec) state ->
                      let key = Reference.Field (objid, prop) in
                      let ver = ReferenceMap.find key versions in
                        if VersionedReferenceMap.mem state (key, ver) then
                          state
                        else
                          add_value (VersionedReferenceMap.add state (key, ver) value) value)
      (BatDynArray.get objects (get_object_id objid)) state
  and add_value state value =
    match try_objectid_of_jsval value with
      | None -> state
      | Some objid -> add_props state objid
  in add_value state value

let is_argument_binding ({ names; last_arguments }: versions_resolved) name =
  match StringMap.find name names with
    | (Reference.Field (obj, _)) -> Some (get_object_id obj) = last_arguments
    | _ -> false
    | exception Not_found -> false

let pp_versions =
  let open Fmt in
    using ReferenceMap.bindings (list (pair Reference.pp_reference int))

let collect_pointsto_step globals_are_properties objects state (facts: versions_resolved) =
  let mkref = Reference.reference_of_name globals_are_properties facts.names in
  fun (op, _) -> match op with
  | CFunPre { base; args } ->
      let state = add_literal objects facts state args
      in add_literal objects facts state base
  | CLiteral { value } ->
    add_literal objects facts state value
  | CDeclare { name; declaration_type = ArgumentBinding _ }
    when is_argument_binding facts name ->
    state
  | CDeclare { name; value } ->
    (* Note that this also catches ArgumentBinding cases where the name is *)
    (* not an alias. *)
    add_write facts state (mkref name) value false
  | CGetField { actual_base; offset; value } ->
    add_read facts state (Reference.reference_of_field actual_base offset) value
  | CPutField { actual_base; offset; value } ->
    add_write facts state (Reference.reference_of_field actual_base offset) value false
  | CRead { name; value } ->
    add_read facts state (mkref name) value
  | CWrite { name; value } ->
    add_write facts state (mkref name) value false
  | CFunEnter { args; this } ->
    let state = add_literal objects facts state args in
    let state = add_write facts state (mkref "this") this true in
      add_literal objects facts state this
  | _ -> state

let globals_points_to (objects: objects) globals versions pt =
  let step ref ver pt =
    let vref = (ref, ver)
    and value = let open Reference in match ref with
      | Field (obj, field) ->
        begin
          try (StringMap.find field (BatDynArray.get objects (get_object_id obj))).value
          with Not_found ->
            failwith ("Can't find field " ^ field ^ " of " ^
                      (Fmt.to_to_string pp_objectid obj))
        end
      | Variable (Global, name) ->
        begin try StringMap.find name globals
          with Not_found ->
            failwith ("Can't  find global variable "^ name) end
      | Variable (Local _, name) ->
        failwith ("Unexpected local variable " ^ name) in
    VersionedReferenceMap.add pt vref value
  in
  ReferenceMap.fold step versions pt

let initial_pointsto init =
  let open Reference in
  let versions =
    CalculateVersions.initial_versions
      init.objects init.globals init.globals_are_properties
  in let vmap = VersionedReferenceMap.empty ()
  in let vmap = VersionedReferenceMap.add vmap (Reference.Variable (Global, "this"), 0) (OObject 0)
  in globals_points_to init.objects init.globals versions vmap

open Reference
let insert_points_to
      ({ last_arguments; closures; last_update; versions; names; prototypes }: versions_resolved)
      points_to: local_facts =
  { last_arguments; closures; last_update; versions; names; points_to; prototypes }

let update_points_to (init: initials) points_to (op, lf) =
  let points_to' =
    collect_pointsto_step init.globals_are_properties init.objects points_to lf op
  in ((op, insert_points_to lf points_to'), points_to')

module Make(S: Streaming.Transformers) = struct
  type 'a trace = 'a S.sequence
  let collect init tr =
    S.map_state (initial_pointsto init) (update_points_to init) tr
end;;

