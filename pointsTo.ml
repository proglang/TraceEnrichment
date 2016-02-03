open TraceTypes
module VersionReferenceMap = Reference.VersionReferenceMap

let find_object_facts id ver pt =
  let vrefs = Reference.ReferenceMap.fold (fun ref ver acc ->
      match ref with
      | Reference.Field (id', fld) when id = id' -> ((ref, ver), fld) :: acc
      | _ -> acc) ver.versions [] in
  List.fold_left (fun acc (vref, fld) ->
      if VersionReferenceMap.mem vref pt then
        StringMap.add fld (VersionReferenceMap.find vref pt) acc
      else begin
        Format.eprintf "vref not found in points-to: %a@." Reference.pp_versioned_reference vref;
        raise Not_found
      end)
    StringMap.empty vrefs


