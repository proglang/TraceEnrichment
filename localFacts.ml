open Misc.Notations
open Types
open Streaming
open TraceTypes

module GenericCollectArguments = functor(S: Transformers) -> struct
  let collect_arguments =
    S.map_state []
      (fun stack op ->
         let stack' = match op with
           | CFunEnter { args } -> get_object args :: stack
           | CFunExit _ -> List.tl stack
           | _ -> stack
         in ((op, Misc.hd_err stack'), stack'))

end;;

module StreamCollectArguments = GenericCollectArguments(StreamTransformers)
module ListCollectArguments = GenericCollectArguments(ListTransformers)

let collect_arguments_stream = StreamCollectArguments.collect_arguments
let collect_arguments_trace = ListCollectArguments.collect_arguments
let collect_arguments_tracefile (f, o, t, g, p) = (f, o, collect_arguments_trace t, g, p)

let reference_of_variable gap facts global name =
  Reference.reference_of_name gap facts.aliases global name

let make_versioned state ref =
  try (ref, Reference.ReferenceMap.find ref state.versions)
  with Not_found -> (Format.eprintf "Did not find %a in %a"
                       Reference.pp_reference ref pp_local_facts state; raise Not_found)


