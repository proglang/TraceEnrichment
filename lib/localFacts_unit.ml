open LocalFacts
open Kaputt.Abbreviations
open TestBaseData
open TypesJS
open TraceTypes

let (|>) = Pervasives.(|>)

let step_number_collector num () op = (num, num+1)
let rec seq start num = if num > 0 then start :: seq (start + 1) (num - 1) else []

let pp_argtrace =
  Fmt.vbox (Fmt.list (Fmt.pair pp_clean_operation (Fmt.option Fmt.int)))

let test_collect_arguments_and_parameters =
  Test.make_simple_test ~title:"collect_arguments_and_parameters"
    (fun () ->
       let argtrace = LocalFacts.collect_arguments_trace cleantrace1 in
       Assert.make_equal (=)
         (Fmt.to_to_string pp_argtrace)
         argtrace1 argtrace)

let test_calculate_arguments_and_parameters =
  Test.make_simple_test ~title:"calculate_arguments_and_parameters"
    (fun () ->
       let (funcs, objs, argtrace, globals', gap) = 
         LocalFacts.collect_arguments_tracefile (functab1, objtab1, cleantrace1, globals, true) in
       Assert.same functab1 funcs;
       Assert.same objtab1 objs;
       Assert.same globals globals';
       Assert.is_true gap;
       Assert.make_equal (=)
         (Fmt.to_to_string pp_argtrace)
         argtrace1 argtrace)

let ref0 = Reference.reference_of_local_name "z"
let alias_map = {
  last_arguments = None;
  last_update = None;
  versions = Reference.ReferenceMap.empty |> Reference.ReferenceMap.add ref0 42;
  aliases = (let open StringMap in empty |> add "y" (TypesJS.Object 1, "a"));
  points_to = Reference.VersionedReferenceMap.empty
}

let test_make_versioned =
  Test.make_simple_test ~title: "make_versioned"
    (fun () ->  Assert.make_equal (=)
        (Fmt.to_to_string Reference.pp_versioned_reference)
        (ref0, 42) (make_versioned alias_map ref0))

let tests = [
    test_collect_arguments_and_parameters; test_calculate_arguments_and_parameters;
    test1; test2; test3; test4; test5; test_make_versioned ] 
