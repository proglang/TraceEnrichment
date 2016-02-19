open PointsTo
open Kaputt.Abbreviations
open Types
open TestBaseData

let (|>) = Pervasives.(|>)

let test_find_object_facts =
  Test.make_simple_test ~title:"find_object_facts" (fun () ->
      let facts = find_object_facts (objectid_of_jsval obj1_simp1) (List.hd (List.rev cleantrace1_facts)) trace1_pointsto in
      Assert.make_equal (StringMap.equal (=))
        (Fmt.to_to_string (StringMap.pp pp_jsval))
        (StringMap.empty
         |> StringMap.add "0" vnull
         |> StringMap.add "1" vundef
         |> StringMap.add "marker" vundef
         |> StringMap.add "toString" (OFunction (0,2)))
        facts)

let tests = [ test_find_object_facts ]
