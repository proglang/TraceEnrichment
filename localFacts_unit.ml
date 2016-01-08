open LocalFacts
open Kaputt.Abbreviations
open TestBaseData
open Types
open TraceTypes

let (|>) = Pervasives.(|>)

let step_number_collector num () op = (num, num+1)
let rec seq start num = if num > 0 then start :: seq (start + 1) (num - 1) else []

let test_collect_arguments_and_parameters =
  Test.make_simple_test ~title:"collect_arguments_and_parameters"
    (fun () ->
       let argtrace = LocalFacts.collect_arguments_trace cleantrace1 in
       let open FormatHelper in
       Assert.make_equal (=)
         (Misc.to_string (pp_print_list_lines (pp_print_pair pp_clean_operation (pp_print_option Format.pp_print_int))))
         argtrace1 argtrace)

let test_calculate_arguments_and_parameters =
  Test.make_simple_test ~title:"calculate_arguments_and_parameters"
    (fun () ->
       let (funcs, objs, argtrace, globals', gap) = 
         LocalFacts.collect_arguments_tracefile (functab1, objtab1, cleantrace1, globals, true) in
       let open FormatHelper in
       Assert.same functab1 funcs;
       Assert.same objtab1 objs;
       Assert.same globals globals';
       Assert.is_true gap;
       Assert.make_equal (=)
         (Misc.to_string (pp_print_list_lines (pp_print_pair pp_clean_operation (pp_print_option Format.pp_print_int))))
         argtrace1 argtrace)

let ref0 = Reference.reference_of_local_name "z"
let alias_map = {
  last_arguments = None;
  last_update = None;
  versions = Reference.ReferenceMap.empty |> Reference.ReferenceMap.add ref0 42;
  aliases = (let open Misc.StringMap in empty |> add "y" (Types.Object 1, "a"));
  points_to = Reference.VersionReferenceMap.empty
}

let test1 = let open Reference in
  Test.make_simple_test ~title:"reference_of_variable: global when globals are not properties"
    (fun () ->
       let ref = reference_of_variable false alias_map true "x" in
       Assert.is_true
         ~msg:(Format.asprintf "Expected 'Global(\"x\")', but got '%a'" pp_reference ref)
         (match ref with GlobalVariable "x" -> true | _ -> false))
let test2 = let open Reference in
  Test.make_simple_test ~title:"reference_of_variable: global when globals are properties"
    (fun () ->
       let ref = reference_of_variable true alias_map true "x" in
       Assert.is_true
         ~msg:(Format.asprintf "Expected 'Field(Object 0, \"x\")', but got '%a'" pp_reference ref)
         (match ref with Field(Object 0, "x") -> true | _ -> false))
let test3 = let open Reference in
  Test.make_simple_test ~title:"reference_of_variable: global when globals are properties; alias handling"
    (fun () ->
       let ref = reference_of_variable true alias_map true "y" in
       Assert.is_true
         ~msg:(Format.asprintf "Expected 'Field(Object 0, \"y\")', but got '%a'" pp_reference ref)
         (match ref with Field(Object 0, "y") -> true | _ -> false))
let test4 = let open Reference in
  Test.make_simple_test ~title:"reference_of_variable: local, not an alias"
    (fun () ->
       let ref = reference_of_variable true alias_map false "x" in
       Assert.is_true
         ~msg:(Format.asprintf "Expected 'Local \"x\"', but got '%a'" pp_reference ref)
         (match ref with LocalVariable "x" -> true | _ -> false))
let test5 = let open Reference in
  Test.make_simple_test ~title:"reference_of_variable: local name; alias handling"
    (fun () ->
       let ref = reference_of_variable true alias_map false "y" in
       Assert.is_true
         ~msg:(Format.asprintf "Expected 'Field(Object 1, \"a\")', but got '%a'" pp_reference ref)
         (match ref with Field(Object 1, "a") -> true | _ -> false))

let test_make_versioned =
  Test.make_simple_test ~title: "make_versioned"
    (fun () ->  Assert.make_equal (=)
        (Misc.to_string Reference.pp_versioned_reference)
        (ref0, 42) (make_versioned alias_map ref0))

let tests = [
    test_collect_arguments_and_parameters; test_calculate_arguments_and_parameters;
    test1; test2; test3; test4; test5; test_make_versioned ] 
