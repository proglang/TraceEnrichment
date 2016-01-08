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

  (*
(** [parse_tracefile c] parses a JSON trace file from input channel [c] and returns it. *)
(** Parse from JSON string *)
val event_of_string: string -> event
val objectspec_of_string: string -> objectspec
val funcspec_of_string: string -> funcspec
val jsval_of_string: string -> jsval

(** [format_tracefile c t] formats a trace file [c] as JSON and writes it to the
  * output channel [c]. *)
val format_clean_tracefile : out_channel -> clean_tracefile -> unit
val format_rich_tracefile : out_channel -> rich_tracefile -> unit
val format_rich_tracefile_small : out_channel -> rich_tracefile -> unit
(** Format as JSON string *)
val string_of_event: event -> string
val string_of_objectspec: objectspec -> string
val string_of_funcspec: funcspec -> string
val string_of_jsval: jsval -> string
val string_of_clean_event : clean_operation -> string
val string_of_rich_operation : rich_operation -> string
val string_of_rich_event : rich_event -> string
val string_of_rich_event_small : rich_event -> string
val string_of_points_to : Reference.points_to_map -> string


val format_objects : objects -> Yojson.Basic.json
val format_functions : functions -> Yojson.Basic.json
val format_globals : globals -> Yojson.Basic.json
val format_objectid : objectid -> Yojson.Basic.json
val format_fieldref : fieldref -> Yojson.Basic.json
val format_trace : trace -> Yojson.Basic.json
val format_clean_event : clean_operation -> Yojson.Basic.json
val format_reference : Reference.reference -> Yojson.Basic.json
val format_versioned_reference : Reference.versioned_reference -> Yojson.Basic.json
val format_rich_operation : rich_operation -> Yojson.Basic.json
val format_rich_event : rich_event -> Yojson.Basic.json
val format_rich_event_small : rich_event -> Yojson.Basic.json
val format_points_to : Reference.points_to_map -> Yojson.Basic.json
   *)

let tests =
  test_parse_tracefile ::
  tests_parse_jsval @
  tests_parse_objectspec @
  tests_parse_funcspec @
  tests_parse_operation
