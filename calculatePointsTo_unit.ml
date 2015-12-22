open Kaputt.Abbreviations
open Types
open Test_base_data

let (|>) = Pervasives.(|>)

let test_calculate_pointsTo =
  Test.make_simple_test ~title:"calculate_versions" (fun () ->
      let pt = ref Reference.VersionReferenceMap.empty in
      let tf1 = (functab1, objtab1, facttrace1, globals, true) in
      let tf2 = CalculatePointsTo.calculate_pointsto pt tf1 in
      let module VRFmt = FormatHelper.MapFormat(Reference.VersionReferenceMap) in
      Assert.make_equal (Reference.VersionReferenceMap.equal (=))
        (Misc.to_string (VRFmt.pp_print_map_default Reference.pp_versioned_reference pp_jsval))
        trace1_pointsto !pt;
      same_facts_tracefile tf1 tf2)

let _ = Test.run_tests [ test_calculate_pointsTo ]
