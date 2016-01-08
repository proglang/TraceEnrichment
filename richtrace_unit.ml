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
  Misc.StringMap.equal (=) facts1.aliases facts2.aliases


let test1 = Test.make_simple_test ~title:"calculate_rich_tracefile" (fun () ->
    let rt = RichTrace.calculate_rich_tracefile (functab1, objtab1, facttrace1, globals, true) in
    Assert.make_equal ~msg:"functions" (=) (Misc.to_string pp_functions) functab1 rt.funcs;
    Assert.make_equal ~msg:"objects" (=) (Misc.to_string pp_objects) objtab1 rt.objs;
    Assert.make_equal ~msg:"globals" (=) (Misc.to_string pp_globals) globals rt.globals;
    Assert.equal_bool ~msg:"globals-are-properties" true rt.globals_are_properties;
    (* Note that the points-to map is not being filled in here! *)
    Assert.make_equal_list ~msg:"trace" rop_eq
      (Misc.to_string (FormatHelper.pp_print_pair pp_rich_operation pp_local_facts))
      richtrace1 rt.trace)

let test2 = Test.make_simple_test ~title:"calculate_rich_tracefile" (fun () ->
    let rt = RichTrace.tracefile_to_rich_tracefile tracefile1 in
    Assert.make_equal ~msg:"functions" (=) (Misc.to_string pp_functions) functab1 rt.funcs;
    Assert.make_equal ~msg:"objects" (=) (Misc.to_string pp_objects) objtab1 rt.objs;
    Assert.make_equal ~msg:"globals" (=) (Misc.to_string pp_globals) globals rt.globals;
    Assert.equal_bool ~msg:"globals-are-properties" true rt.globals_are_properties;
    let module VRMFmt = FormatHelper.MapFormat(Reference.VersionReferenceMap) in
    Assert.make_equal ~msg:"points-to map" (Reference.VersionReferenceMap.equal (=))
      (Misc.to_string (VRMFmt.pp_print_map_default Reference.pp_versioned_reference pp_jsval))
      trace1_pointsto rt.points_to;
    Assert.make_equal_list ~msg:"trace" rop_eq
      (Misc.to_string (FormatHelper.pp_print_pair pp_rich_operation pp_local_facts))
      richtrace1 rt.trace)

let tests = [ test1; test2 ]

