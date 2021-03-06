open Kaputt.Abbreviations

let () =
  Log.default_setup true;
  Test.run_tests
          (CalculatePointsTo_unit.tests @
           CalculateVersions_unit.tests @
           Cleantrace_unit.tests @
           LocalFacts_unit.tests @
           PointsTo_unit.tests @
           Reference_unit.tests @
           Richtrace_unit.tests @
           Trace_unit.tests)

