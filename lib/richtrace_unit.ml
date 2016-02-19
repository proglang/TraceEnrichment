open Types
open Kaputt.Abbreviations
open TestBaseData
open TraceTypes
open LocalFacts

let (|>) = Pervasives.(|>)

let rop_eq (op1, facts1) (op2, facts2) =
  op1 = op2 &&
  facts1.last_arguments = facts2.last_arguments &&
  Reference.ReferenceMap.equal (=) facts1.versions facts2.versions &&
  StringMap.equal (=) facts1.aliases facts2.aliases


let test1 = Test.make_simple_test ~title:"calculate_rich_tracefile" (fun () ->
    let rt = RichTrace.calculate_rich_tracefile (functab1, objtab1, facttrace1, globals, true) in
    Assert.make_equal ~msg:"functions" (=) (Fmt.to_to_string pp_functions) functab1 rt.funcs;
    Assert.make_equal ~msg:"objects" (=) (Fmt.to_to_string pp_objects) objtab1 rt.objs;
    Assert.make_equal ~msg:"globals" (=) (Fmt.to_to_string pp_globals) globals rt.globals;
    Assert.equal_bool ~msg:"globals-are-properties" true rt.globals_are_properties;
    (* Note that the points-to map is not being filled in here! *)
    Assert.make_equal_list ~msg:"trace" rop_eq
      (Fmt.to_to_string (Fmt.pair pp_rich_operation pp_local_facts))
      richtrace1 rt.trace)

let test2 = Test.make_simple_test ~title:"calculate_rich_tracefile" (fun () ->
    let rt = RichTrace.tracefile_to_rich_tracefile tracefile1 in
    Assert.make_equal ~msg:"functions" (=) (Fmt.to_to_string pp_functions) functab1 rt.funcs;
    Assert.make_equal ~msg:"objects" (=) (Fmt.to_to_string pp_objects) objtab1 rt.objs;
    Assert.make_equal ~msg:"globals" (=) (Fmt.to_to_string pp_globals) globals rt.globals;
    Assert.equal_bool ~msg:"globals-are-properties" true rt.globals_are_properties;
    Assert.make_equal ~msg:"points-to map" (Reference.VersionReferenceMap.equal (=))
      (Fmt.to_to_string Reference.pp_points_to_map)
      trace1_pointsto rt.points_to;
    Assert.make_equal_list ~msg:"trace" rop_eq
      (Fmt.to_to_string (Fmt.pair pp_rich_operation pp_local_facts))
      richtrace1 rt.trace)

let tests = [ test1; test2 ]

