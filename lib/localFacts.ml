open Types
open Streaming
open TraceTypes

type arguments_and_closures = {
  (** The last argument object that was created by a function call. *)
  last_arguments: int option;
  (** Closure enviroments for functions. *)
  closures: int IntMap.t
}
type names_resolved = {
  (** The last argument object that was created by a function call. *)
  last_arguments: int option;
  (** Closure enviroments for functions. *)
  closures: int IntMap.t;
  (** All visible variable names. *)
  names: Reference.reference StringMap.t
}
type versions_resolved = {
  (** The last argument object that was created by a function call. *)
  last_arguments: int option;
  (** Closure enviroments for functions. *)
  closures: int IntMap.t;
  (** The last reference that was modified. *)
  last_update: Reference.versioned_reference option;
  (** The current version of all known references. *)
  versions: int Reference.ReferenceMap.t;
  (** All visible variable names. *)
  names: Reference.reference StringMap.t;
}
type local_facts = {
  (** The last argument object that was created by a function call. *)
  last_arguments: int option;
  (** Closure enviroments for functions. *)
  closures: int IntMap.t;
  (** The last reference that was modified. *)
  last_update: Reference.versioned_reference option;
  (** The current version of all known references. *)
  versions: int Reference.ReferenceMap.t;
  (** All visible variable names. *)
  names: Reference.reference StringMap.t;
  (** The current state of the points-to map. *)
  points_to: Reference.points_to_map
}

module GenericCollectArguments = functor(S: Transformers) -> struct
  let collect_arguments =
    S.map_state []
      (fun stack op ->
         let stack' = match op with
           | CFunEnter { args } -> get_object args :: stack
           | CFunExit _ -> List.tl stack
           | _ -> stack
         in ((op, BatList.Exceptionless.hd stack'), stack'))

end
module GenericCollectClosures = functor(S: Transformers) -> struct
  let collect =
    S.map_state IntMap.empty (fun closures (op, arg) ->
             match op, arg with
               | CLiteral { value = OFunction(id, _) }, Some arg ->
                   let closures' = IntMap.add id arg closures in
                   ((op, { last_arguments = Some arg; closures =  closures' }), closures')
               | _, _ ->
                   ((op, { last_arguments = arg; closures }), closures))
end

let reference_of_variable gap facts name =
  Reference.reference_of_name gap facts.names name

let pp_local_facts pp { last_arguments; last_update; versions; names; points_to } =
  Format.fprintf pp "@[< v >\
                     Last callee-side argument object: %a@ \
                     Last update: %a@ \
                     Versions: @[< hov 2 >%a@]@ \
                     Aliases: @[< hov 2 >%a@]@ \
                     Points-to map: @[< hov 2 >%a@]@ @]"
    (Fmt.option Fmt.int) last_arguments
    (Fmt.option Reference.pp_versioned_reference) last_update
    (Reference.pp_reference_map Fmt.int) versions
    (StringMap.pp (*~entry_sep:(Fmt.const Fmt.string " -> ")*) Reference.pp_reference) names
    Reference.pp_points_to_map points_to

let pp_facts_trace = pp_enriched_trace pp_local_facts
let pp_facts_tracefile = pp_enriched_tracefile pp_local_facts

let make_versioned_impl versions ref =
  try (ref, Reference.ReferenceMap.find ref versions)
  with Not_found -> (Format.eprintf "Did not find %a in %a"
                       Reference.pp_reference ref
                       (Reference.pp_reference_map Fmt.int) versions; raise Not_found)

let make_versioned state ref = make_versioned_impl state.versions ref

