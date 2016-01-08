open Kaputt.Abbreviations
open Types
open TestBaseData

let (|>) = Pervasives.(|>)

let test_calculate_pointsTo =
  Test.make_simple_test ~title:"calculate_versions" (fun () ->
      let tf1 = (functab1, objtab1, facttrace1, globals, true) in
      let tf2 = CalculatePointsTo.calculate_pointsto tf1 in
      let points_to =
        let (_, _, trace, _, _) = tf2 in
          trace |> List.rev |> List.hd |> snd |> fun { TraceTypes.points_to } -> points_to in
      let module VRFmt = FormatHelper.MapFormat(Reference.VersionReferenceMap) in
      Assert.make_equal (Reference.VersionReferenceMap.equal (=))
        (Misc.to_string (VRFmt.pp_print_map_default Reference.pp_versioned_reference pp_jsval))
        trace1_pointsto points_to;
      same_facts_tracefile tf1 tf2)

let tests = [ test_calculate_pointsTo ]
