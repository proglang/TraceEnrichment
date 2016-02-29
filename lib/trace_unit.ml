open Types
open TraceTypes
open Trace
open TestBaseData
open Kaputt.Abbreviations
open Kaputt

let test_parse_tracefile =
  Test.make_assert_test ~title:"parse tracefile"
    (fun () -> Filename.open_temp_file "testtrace" ".json")
    (fun (name, out) ->
       format_tracefile out tracefile1;
       close_out out;
       let inf = open_in name in
       same_tracefile tracefile1 (parse_tracefile inf);
       close_in inf;
       name)
    (fun name -> Sys.remove name)

let tests_parse_jsval =
  List.map (fun v ->
              Test.make_simple_test ~title:"jsval round-trip"
                (fun () ->
                   Assert.make_equal (=) string_of_jsval v (parse_jsval (format_jsval v))))
    [ OUndefined;
      ONull;
      ONumberFloat 1.0;
      ONumberInt 42;
      OBoolean true;
      OBoolean false;
      OString "rawra3ra3r";
      OSymbol "rjq43r0q3r";
      OObject 17;
      OFunction (69, 105);
      OOther ("rr3ar32", 23) ]

let tests_parse_objectspec =
  List.map (fun o ->
              Test.make_simple_test ~title:"objectspec round-trip"
                (fun () ->
                   same_objectspec o (parse_objectspec (format_objectspec o))))
    [ obj1desc_cyc1; obj1desc_cyc2; obj1desc_cyc3;
      obj1desc_list1; obj1desc_list2; obj1desc_list3;
      obj1desc_fun1; obj1desc_fun2; obj1desc_fun3; obj1desc_fun4;
      obj1desc_simp1; obj1desc_simp2; obj1desc_special ]

let tests_parse_funcspec =
  List.map (fun f ->
              Test.make_simple_test ~title:"funcspec round-trip"
                (fun () ->
                   same_funcspec f (parse_funcspec (format_funcspec f))))
    [ funcapply; funccall; functostring; funcin1; funcin2; funcstd; funcext1 ]

let tests_parse_operation =
  List.map (fun o ->
              Test.make_simple_test ~title:("operation round-trip")
                (fun () -> Assert.make_equal (=) string_of_event o
                             (parse_operation (format_event o))))
    trace1

let tests =
  test_parse_tracefile ::
  tests_parse_jsval @
  tests_parse_objectspec @
  tests_parse_funcspec @
  tests_parse_operation
