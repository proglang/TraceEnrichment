open Types
open TraceTypes
module VersionReferenceMap = Reference.VersionReferenceMap
module ReferenceMap = Reference.ReferenceMap
type points_to_map = Reference.points_to_map

let add_write facts state ref value may_be_known: points_to_map =
  let vref = LocalFacts.make_versioned facts ref in
    if VersionReferenceMap.mem vref state then begin
      (* This write was dropped; most likely, the field was marked
       * "not writable". *)
      if not may_be_known then
        Format.eprintf
          "Weirdness detected: Write of %a failed (ref: %a)@."
          Reference.pp_reference ref
          Reference.pp_versioned_reference vref;
      state
    end else
      VersionReferenceMap.add vref value state

let add_read facts state ref value: points_to_map =
  let vref = LocalFacts.make_versioned facts ref in
  if VersionReferenceMap.mem vref state then begin
    if (value <> VersionReferenceMap.find vref state) then begin
      Format.eprintf
        "Weirdness detected: In read of %a, expected %a, but got %a@."
        Reference.pp_reference ref
        pp_jsval (VersionReferenceMap.find vref state)
        pp_jsval value
    end;
    state
  end else
    VersionReferenceMap.add vref value state

let add_known_new_object objects facts state obj =
  Debug.debug "Adding known object %a@." pp_jsval obj;
  let id = objectid_of_jsval obj in
  let may_be_known = (Some (get_object_id id) = facts.last_arguments) in
  StringMap.fold
    (fun name (objspec: fieldspec) state ->
       add_write facts state
         (Reference.reference_of_fieldref (id, name))
         objspec.value may_be_known)
    (BatDynArray.get objects(get_object_id id))
    state

let add_literal objects facts state value =
  (* HACK use the fact that all references in state.versions should be *)
  (* mapped to find missing fields.  *)
  ReferenceMap.fold
    (fun ref ver state ->
       let vref = (ref, ver) in
       if VersionReferenceMap.mem vref state then
         state
       else
         match ref with
         | Reference.Field (obj, field) ->
           let objid = get_object_id obj in
           let ({ value }: fieldspec) =
             StringMap.find field (BatDynArray.get objects objid)
           in
           VersionReferenceMap.add vref value state
         | Reference.LocalVariable name
         | Reference.GlobalVariable name ->
             failwith ("Unexpected unmapped variable " ^ name))
    facts.versions
    state

let is_alias { aliases } name = StringMap.mem name aliases

let pp_versions =
  let open Fmt in
    using ReferenceMap.bindings (list (pair Reference.pp_reference int))

let collect_pointsto_step globals_are_properties objects state facts =
  fun step -> Debug.debug "points-to collection step: %a, %a@."
                pp_clean_operation step
                pp_versions facts.versions;
              step |>
  function
  | CFunPre { base; args } ->
      let state = add_known_new_object objects facts state args
      in add_literal objects facts state base
  | CLiteral { value } ->
    add_literal objects facts state value
  | CDeclare { name; declaration_type = ArgumentBinding _ }
    when is_alias facts name ->
    state
  | CDeclare { name; value } ->
    (* Note that this also catches ArgumentBinding cases where the name is *)
    (* not an alias. *)
    add_write facts state (Reference.reference_of_local_name name) value false
  | CGetField { base; offset; value } ->
    add_read facts state (Reference.reference_of_field base offset) value
  | CPutField { base; offset; value } ->
    add_write facts state (Reference.reference_of_field base offset) value false
  | CRead { name; value; isGlobal } ->
    let ref =
      LocalFacts.reference_of_variable globals_are_properties facts isGlobal name
    in add_read facts state ref value
  | CWrite { name; value; isGlobal } ->
    let ref =
      LocalFacts.reference_of_variable globals_are_properties facts isGlobal name
    in add_write facts state ref value false
  | CFunEnter { args; this } ->
    let state = add_known_new_object objects facts state args in
    let state = add_write facts state (Reference.reference_of_local_name "this") this true in
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
      | GlobalVariable name ->
        begin try StringMap.find name globals
          with Not_found ->
            failwith ("Can't  find global variable "^ name) end
      | LocalVariable name ->
        failwith ("Unexpected local variable " ^ name) in
    VersionReferenceMap.add vref value pt
  in
  ReferenceMap.fold step versions pt

let initial_pointsto init =
  let open Reference in
  let versions =
    CalculateVersions.initial_versions
      init.objects init.globals init.globals_are_properties
  in (VersionReferenceMap.empty
        |> VersionReferenceMap.add (Reference.reference_of_local_name "this", 0) (OObject 0)
        |> globals_points_to init.objects init.globals versions)

open Reference
let update_points_to (init: initials) points_to (op, lf) =
  let points_to' =
    collect_pointsto_step init.globals_are_properties init.objects points_to lf op
  in ((op, { lf with points_to = points_to' }), points_to')

module GenericPointsTo(S: Streaming.Transformers) = struct
  let calculate init tr =
    S.map_state (initial_pointsto init) (update_points_to init) tr
end;;

module ListPointsTo = GenericPointsTo(Streaming.ListTransformers);;
module StreamPointsTo = GenericPointsTo(Streaming.StreamTransformers);;

let calculate_pointsto (functions, objects, trace, globals, globals_are_properties) =
  let init = 
    { functions; objects; globals_are_properties; globals }
  in let trace' = ListPointsTo.calculate init trace
  in (functions, objects, trace', globals, globals_are_properties)

let collect_pointsto_stream = StreamPointsTo.calculate
