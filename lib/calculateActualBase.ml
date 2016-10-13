open Reference
open TypesJS
open Streaming
open TraceTypes

let rec find_prototype objects prototypes obj field =
  if StringMap.mem field (BatDynArray.get objects (get_object_id obj)) then
    obj
  else find_prototype objects prototypes (ObjectIDMap.find obj prototypes) field

let set_actual_base objects prototypes ({ base; offset } as acc) =
  let actual_base =
    try
      find_prototype objects prototypes (objectid_of_jsval base) offset |>
        objectid_to_jsval
    with Not_found -> base
  in { acc with actual_base }

let update_ops objects ({ LocalFacts.prototypes }: LocalFacts.prototypes_resolved) = function
  | (CGetField acc, sid) -> (CGetField (set_actual_base objects prototypes acc), sid)
  | (CPutField acc, sid) -> (CPutField (set_actual_base objects prototypes acc), sid)
  | op -> op

module type S = sig
  type 'a trace
  val collect: initials ->
    (clean_event * LocalFacts.prototypes_resolved) trace ->
    (clean_event * LocalFacts.prototypes_resolved) trace
end
module Make (T: Transformers) = struct
  type 'a trace = 'a T.sequence
  let collect {objects} =
    T.map (fun (op, facts) -> (update_ops objects facts op, facts))
end

