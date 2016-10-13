open Reference
open TypesJS
open Streaming
open TraceTypes

let rec collect_prototypes_for (objects: objects) prototypes (obj: objectid) =
  if ObjectIDMap.mem obj prototypes then
    prototypes
  else
    (* Look up the prototype field *)
    match (StringMap.Exceptionless.find "prototype"
             (BatDynArray.get objects (get_object_id obj)): fieldspec option)
    with
      | Some { value = ONull }
      | Some { value = OUndefined } ->
          (* No prototype, return *)
          prototypes
      | Some { value = proto } ->
          let obj' = objectid_of_jsval proto
          in collect_prototypes_for objects (ObjectIDMap.add obj obj' prototypes) obj'
      | None ->
          Log.debug (fun m -> m "No prototype for object %a" pp_objectid obj);
          prototypes

let collect_prototypes_if_needed objects value prototypes =
  if is_base value then
    prototypes
  else
    collect_prototypes_for objects prototypes (objectid_of_jsval value)

let update_prototypes objects prototypes =
  let collect = collect_prototypes_if_needed objects in function
    | CFunPre { f; base; args } ->
        prototypes |> collect f |> collect base |> collect args
    | CFunPost { result } -> collect result prototypes
    | CLiteral { value } -> collect value prototypes
    | CDeclare { value } -> collect value prototypes
    | CGetFieldPre (value, _) -> collect value prototypes
    | CPutFieldPre { base; value }
    | CGetField { base; value } ->
        prototypes |> collect base |> collect value
    | CPutField { base; value; offset } ->
        let prototypes = prototypes |> collect base |> collect value
        in if offset = "prototype" then begin
          (* TODO the true semantics are a bit more involved. *)
          try
            ObjectIDMap.add (objectid_of_jsval base) (objectid_of_jsval value) prototypes
          with NotAnObject ->
            Log.debug (fun m ->
                         m "Trying to assign prototype of %a to be %a, at least one is not an object"
                           pp_jsval base pp_jsval value);
            prototypes
        end else prototypes
    | CRead { value } -> collect value prototypes
    | CWrite { lhs } -> collect lhs prototypes
    | CFunEnter { this; args } ->
        prototypes |> collect this |> collect args
    | CFunExit { ret; exc } -> prototypes |> collect ret |> collect exc
    | _ -> prototypes

let collect_prototypes_step
      (objects: objects) prototypes (facts: LocalFacts.names_resolved) (op, sid) =
  let open LocalFacts in
  let prototypes = update_prototypes objects prototypes op
  in (((op, sid),
       { last_arguments = facts.last_arguments;
         closures = facts.closures;
         names = facts.names;
         prototypes }),
      prototypes)

(* The following is a bit of a hack. Explanation:
 * Since we haven't saved the object type in the object array,
 * for the top-level objects, we have to guess the type. Then again,
 * just using Object does no harm here: At worst, the prototype
 * map is polluted with a few extra entries that do not get used.
 * Note that the types of non-toplevel objects will be correct, so this
 * will affect very few objects; indeed, it should at most affect the
 * global object by the way how the initial object array is constructed. *)
let initial_prototypes objs =
  let prototypes = ref ObjectIDMap.empty
  in for i = 0 to BatDynArray.length objs - 1 do
    prototypes := collect_prototypes_for objs !prototypes (Object i)
  done; !prototypes

module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_event * LocalFacts.names_resolved) trace ->
    (clean_event * LocalFacts.prototypes_resolved) trace
end
module Make (T: Transformers) = struct
  type 'a trace = 'a T.sequence
  let collect {objects; globals_are_properties; globals} =
    T.map_state
      (initial_prototypes objects)
      (fun state (op, facts) -> collect_prototypes_step objects state facts op)
end

