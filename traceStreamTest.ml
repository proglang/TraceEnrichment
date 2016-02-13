open Kaputt
open Kaputt.Abbreviations
open Types
open TraceTypes
open Yojson.Basic

let (|>) = Pervasives.(|>)

let get_data () =
  match from_file "traceStreamTest.json" |> Util.to_list |> List.map to_string with
    | init :: facts -> (init, facts)
    | _ -> failwith "Bad test case"

exception InvalidItem

module StringMapEqual =
  Assert.Map(StringMap)(struct type t = string let to_string s: string = s end)
let test_init =
  Test.make_assert_test get_data
    (fun (init, _) ->
       let ((initials: initials), _, _) = TraceStream.parse_setup_packet init in
         Assert.is_true ~msg:"globals_are_properties" initials.globals_are_properties;
         Assert.equal_int ~msg:"objects" 0 (BatDynArray.length initials.objects);
         Assert.equal_int ~msg:"functions" 0 (BatDynArray.length initials.functions);
         StringMapEqual.make_equal (=) (Fmt.to_to_string pp_jsval)
           (StringMap.add "object" (OObject 0) StringMap.empty) initials.globals)
    (fun () -> ())

let fspec jsval = { value = jsval; writable = true; get = None; set = None; enumerable = true; configurable = true }
let test_full =
  Test.make_assert_test get_data
    (fun (init, facts) ->
       let ((initials: initials), stream, push) = TraceStream.parse_setup_packet init in
       let trace = Lwt_stream.to_list stream in
         List.iter push facts;
         let trace' = Lwt_main.run trace in
           Assert.is_true ~msg:"globals_are_properties" initials.globals_are_properties;
           StringMapEqual.make_equal (=) (Fmt.to_to_string pp_jsval)
             (StringMap.add "object" (OObject 0) StringMap.empty) initials.globals;
           Assert.make_equal_list (=) (Fmt.to_to_string pp_objectspec)
             [ StringMap.of_list [ ("Array", fspec (OFunction (1,0))); ("console", fspec (OObject 2)) ];
               StringMap.of_list [ ("prototype", fspec (OObject 1)) ];
               StringMap.empty;
               StringMap.empty ]
             (BatDynArray.to_list initials.objects);
           Assert.make_equal_list (=) (Fmt.to_to_string pp_funcspec)
             [ External 0; Local { from_toString = "/* instrumented */";
                                   from_jalangi = Some "/* uninstrumented */" } ]
             (BatDynArray.to_list initials.functions);
           Assert.make_equal_list (=) (Fmt.to_to_string pp_operation)
             [ Literal (42, { value = OFunction (3, 1); hasGetterSetter = false }) ]
             trace')
    (fun () -> ())

let _ = Test.run_tests [ test_init; test_full ]
