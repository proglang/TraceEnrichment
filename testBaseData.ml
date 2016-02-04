(** Basic data for tests. *)
open Types
open TraceTypes

(** Common instances of JavaScript values - base instances. *)
let vundef = OUndefined
let vnull = ONull
let vtrue = OBoolean true
let vfalse = OBoolean false
let v0 = ONumberInt 0
let v1 = ONumberInt 1
let v2 = ONumberInt 2
let vpi = ONumberFloat 3.14
let vsqrt2 = ONumberFloat 1.41
let valpha = OString "alpha"
let vbeta = OString "beta"
let vmain = OSymbol "main"

(** Three versions of function tables, two equivalent but permuted, and one different. *)
let funcin1 = Local { from_toString = "func 1 toString"; from_jalangi = Some "func 1 jalangi" }
let funcin2 = Local { from_toString = "func 2 toString"; from_jalangi = Some "func 2 jalangi" }
let funcin3 = Local { from_toString = "func 3 toString"; from_jalangi = Some "func 3 jalangi" }
let funcstd = Local { from_toString = "func std toString"; from_jalangi = None }
let funcext1 = External 1
let funcext2 = External 2
let funcapply = External 100
let funccall = External 101
let functostring = External 102

let functab1 = [| funcapply; funccall; functostring; funcin1; funcin2; funcstd; funcext1 |] |> BatDynArray.of_array
(** functab2 is a cyclic permutation of functab1 *)
let functab2 = [| funcapply; funccall; functostring; funcin2; funcstd; funcext1; funcin1 |] |> BatDynArray.of_array
(** functab3 is distinct from functab1 and functab2 *)
let functab3 = [| funcapply; funccall; functostring; funcin1; funcin3; funcstd; funcext2 |] |> BatDynArray.of_array

(* Indices:*
 * 0		apply			apply			apply
 * 1		call			call			call
 * 2		toString	toString	toString
 * 3		in1				in2				in1
 * 4		in2				std				in3
 * 5		std				ext1			std
 * 6		ext1			in1				ext2
*)

(** Helpers for object table generation *) 
let simplefields fields =
  ("toString", OFunction(0, 2)) :: fields
  |> List.map (fun (k, v) ->
      (k, { value = v;  writable = true; get = None; set = None; enumerable = true; configurable = true }))
  |> List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty

let funcfields = simplefields [ ("call", OFunction(0, 1)); ("apply", OFunction(0, 0)) ]

let objglobal =
  simplefields [
    ("Function", OObject 0); ("prototype", OObject 0);
    ("call", OFunction(0, 1)); ("apply", OFunction(0, 0))
  ]

(** Three versions of object tables, two equivalent but permuted, and one different. *)
(** Object table 1 - it contains one cyclic structure, one list structure, one special object and six
 * simple objects, including the functions defined above. *)
let obj1_cyc1 = OObject 1
let obj1_cyc2 = OObject 2
let obj1_cyc3 = OObject 3
let obj1_list1 = OObject 4
let obj1_list2 = OObject 5
let obj1_list3 = OObject 6
let obj1_fun1 = OFunction (7, 3)
let obj1_fun2 = OFunction (8, 4)
let obj1_fun3 = OFunction (9, 5)
let obj1_fun4 = OFunction (10, 6)
let obj1_simp1 = OObject 11
let obj1_simp2 = OObject 12
let obj1_special = OOther ("special", 13)

let obj1desc_cyc1 =
  simplefields [ ("next", obj1_cyc2); ("val", v0) ]
let obj1desc_cyc2 =
  simplefields [ ("next", obj1_cyc3); ("val", v1) ]
let obj1desc_cyc3 =
  simplefields [ ("next", obj1_cyc1); ("val", v2) ]
let obj1desc_list1 =
  simplefields [ ("next", obj1_list2); ("val", vpi) ]
let obj1desc_list2 =
  simplefields [ ("next", obj1_list3); ("val", vsqrt2) ]
let obj1desc_list3 =
  simplefields [ ("val1", vtrue); ("val2", vfalse) ]
let obj1desc_fun1 = funcfields
let obj1desc_fun2 = funcfields
let obj1desc_fun3 = funcfields
let obj1desc_fun4 = funcfields
let obj1desc_simp1 = simplefields [ ("0", vnull); ("1", vundef) ]
let obj1desc_simp2 = simplefields []
let obj1desc_special =
  StringMap.add "_getter"
    { value = vundef; get = Some obj1_fun1; set = Some obj1_fun1;
      configurable = false; enumerable = false; writable = true }
    StringMap.empty

let objtab1 = [|
  objglobal;
  obj1desc_cyc1; obj1desc_cyc2; obj1desc_cyc3;
  obj1desc_list1; obj1desc_list2; obj1desc_list3;
  obj1desc_fun1; obj1desc_fun2;
  obj1desc_fun3; obj1desc_fun4;
  obj1desc_simp1; obj1desc_simp2; obj1desc_special
|] |> BatDynArray.of_array

(** Object table 2 - it contains one cyclic structure, one list structure, one special object and six
 * simple objects, including the functions defined above. *)
let obj2_cyc1 = OObject 4
let obj2_cyc2 = OObject 5
let obj2_cyc3 = OObject 6
let obj2_list1 = OObject 1
let obj2_list2 = OObject 2
let obj2_list3 = OObject 3
let obj2_fun1 = OFunction (9, 4)
let obj2_fun2 = OFunction (10, 5)
let obj2_fun3 = OFunction (7, 6)
let obj2_fun4 = OFunction (8, 2)
let obj2_simp1 = OObject 13
let obj2_simp2 = OObject 12
let obj2_special = OOther ("special", 11)

let obj2desc_cyc1 =
  simplefields [ ("next", obj2_cyc2); ("val", v0) ]
let obj2desc_cyc2 =
  simplefields [ ("next", obj2_cyc3); ("val", v1) ]
let obj2desc_cyc3 =
  simplefields [ ("next", obj2_cyc1); ("val", v2) ]
let obj2desc_list1 =
  simplefields [ ("next", obj2_list2); ("val", vpi) ]
let obj2desc_list2 =
  simplefields [ ("next", obj2_list3); ("val", vsqrt2) ]
let obj2desc_list3 =
  simplefields [ ("val1", vtrue); ("val2", vfalse) ]
let obj2desc_fun1 = funcfields
let obj2desc_fun2 = funcfields
let obj2desc_fun3 = funcfields
let obj2desc_fun4 = funcfields
let obj2desc_simp1 = simplefields [ ("0", vnull); ("1", vundef) ]
let obj2desc_simp2 = simplefields []
let obj2desc_special =
  StringMap.add "_getter"
    { value = vundef; get = Some obj2_fun1; set = Some obj2_fun1;
      configurable = false; enumerable = false; writable = true }
    StringMap.empty

let objtab2 = [|
  objglobal;
  obj2desc_list1; obj2desc_list2; obj2desc_list3;
  obj2desc_cyc1; obj2desc_cyc2; obj2desc_cyc3;
  obj2desc_fun3; obj2desc_fun4;
  obj2desc_fun1; obj2desc_fun2;
  obj2desc_special; obj2desc_simp2; obj2desc_simp1
|] |> BatDynArray.of_array

(** Object table 3 - similar to object table 1, but with subtle differences
 * in the objects themselves. *)
let obj3_cyc1 = OObject 1
let obj3_cyc2 = OObject 2
let obj3_cyc3 = OObject 3
let obj3_list1 = OObject 4
let obj3_list2 = OObject 5
let obj3_list3 = OObject 6
let obj3_fun1 = OFunction (7, 3)
let obj3_fun2 = OFunction (8, 4)
let obj3_fun3 = OFunction (9, 5)
let obj3_fun4 = OFunction (10, 6)
let obj3_simp1 = OObject 11
let obj3_simp2 = OObject 12
let obj3_special = OOther ("special", 13)

let obj3desc_cyc1 =
  simplefields [ ("next", obj3_cyc2); ("val", vtrue) ]
let obj3desc_cyc2 =
  simplefields [ ("next", obj3_cyc3); ("val", valpha) ]
let obj3desc_cyc3 =
  simplefields [ ("next", obj3_cyc1); ("val", v2) ]
let obj3desc_list1 =
  simplefields [ ("next", obj3_list2); ("val", vpi) ]
let obj3desc_list2 =
  simplefields [ ("next", obj3_list3); ("val", vbeta) ]
let obj3desc_list3 =
  simplefields [ ("val1", vtrue); ("val2", vnull) ]
let obj3desc_fun1 = funcfields
let obj3desc_fun2 = funcfields
let obj3desc_fun3 = funcfields
let obj3desc_fun4 = funcfields
let obj3desc_simp1 = simplefields [ ("0", vtrue); ("1", vundef) ]
let obj3desc_simp2 = simplefields []
let obj3desc_special =
  StringMap.add "_getter"
    { value = vnull; get = Some obj3_fun1; set = Some obj3_fun1;
      configurable = false; enumerable = false; writable = false }
    StringMap.empty

let objtab3 = [|
  objglobal;
  obj3desc_cyc1; obj3desc_cyc2; obj3desc_cyc3;
  obj3desc_list1; obj3desc_list2; obj3desc_list3;
  obj3desc_fun1; obj3desc_fun2;
  obj3desc_fun3; obj3desc_fun4;
  obj3desc_simp1; obj3desc_simp2; obj3desc_special
|] |> BatDynArray.of_array

(** Global maps for the above object arrays. *)
let globals = let open StringMap in empty |> add "Function" (OObject 0)

(** Facts and points-to maps that represent the object tables from above. *)
let get_facts_for (objtab: objects) =
  let add_facts idx (desc: objectspec) acc =
    StringMap.fold (fun field (desc: fieldspec) (local_facts, points_to) ->
        let ref = Reference.reference_of_field (OObject idx) field in
        ({ local_facts with
           versions = Reference.ReferenceMap.add ref 0 local_facts.versions },
         Reference.VersionReferenceMap.add (ref, 0) desc.value points_to))
      desc acc 
  and empty_facts = {
    last_arguments = None;
    last_update = None;
    aliases = StringMap.empty;
    versions = Reference.ReferenceMap.empty;
    points_to = Reference.VersionReferenceMap.empty
  } in
  let foldi f a x =
    snd (BatDynArray.fold_left (fun (i, x) y -> (i+1, f i y x)) (0, x) a)
  in foldi add_facts objtab (empty_facts, Reference.VersionReferenceMap.empty)

let (local_facts_1, points_to_1) = get_facts_for objtab1
let (local_facts_2, points_to_2) = get_facts_for objtab2
let (local_facts_3, points_to_3) = get_facts_for objtab3

(** A all-inclusive, well-bracketed trace exercising all (significant) cases.
 * It is built for object table 1. *)
let trace1 = 
  [
    ForIn (1, obj1_simp2);
    With (2, obj1_simp2);
    ScriptEnter;
    Throw (4, obj1_simp2);
    ScriptExc obj1_simp2;
    Declare (6, { name = "e"; value = obj1_simp2; argument = None; isCatchParam = true  });
    EndExpression 7;
    Literal (8, { value = vtrue; hasGetterSetter = false  });
    Write (9, { name = "x"; lhs = vundef; value = vtrue; isGlobal = true; isScriptLocal = true  });
    Read (10, { name = "x"; value = vtrue; isGlobal = true; isScriptLocal = true  });
    FunPre (11, { f = obj1_fun1; base = obj1_cyc1; args = obj1_simp1; isConstructor = false; isMethod = true  });
    FunEnter (12, { f = obj1_fun1; this = obj1_cyc1; args = obj1_simp1  });
    Declare (12, { name = "arguments"; value = obj1_simp1; argument = (Some (-1)); isCatchParam = false  });
    Declare (12, { name = "x"; value = vundef; argument = Some 0; isCatchParam = false  });
    Return (13, vfalse );
    FunExit (14, { ret = vfalse; exc = vundef  });
    FunPost (15, { f = obj1_fun1; args = obj1_simp1; isConstructor = false; isMethod = true; result = vfalse; base = obj1_cyc1  });
    ScriptEnter;
    BinPre (16, { op = "+"; left = v0; right = v1; isOpAssign = false; isSwitchCaseComparison = false; isComputed = false  });
    BinPost (16, { op = "+"; left = v0; right = v1; isOpAssign = false; isSwitchCaseComparison = false; isComputed = false; result = v1  });
    UnaryPre (17, { op = "-"; arg = v0  });
    UnaryPost (17, { op = "-"; arg = v0; result = v0  });
    ScriptExit;
    GetFieldPre (18, { base = obj1_simp1; offset = "marker"; isComputed = false; isOpAssign = false; isMethodCall = false });
    GetField (18, { base = obj1_simp1; offset = "marker"; isComputed = false; isOpAssign = false; isMethodCall = false; value = vundef });
    PutFieldPre (19, { base = obj1_simp1; offset = "marker"; isOpAssign = false; isComputed =false; value = vundef });
    PutField (19, { base = obj1_simp1; offset = "marker"; isOpAssign = false; isComputed =false; value = vundef });
    Literal (20, { value = obj1_simp2; hasGetterSetter = false  });
    Declare (21, { name = "y"; value = obj1_simp2; argument = None; isCatchParam = false  });
    Conditional vfalse
  ]

let tracefile1: tracefile = (functab1, objtab1, trace1, globals, true)

let cleantrace1 = [
  CForIn obj1_simp2;
  CWith obj1_simp2;
  CScriptEnter;
  CThrow obj1_simp2;
  CScriptExc obj1_simp2;

  CDeclare { name = "e"; value = obj1_simp2; declaration_type = CatchParam };
  CEndExpression;
  CLiteral { value = vtrue; hasGetterSetter = false };
  CWrite { name = "x"; lhs = vundef; value = vtrue; isGlobal = true; isSuccessful = true };
  CRead { name = "x"; value = vtrue; isGlobal = true };

  CFunPre { f = obj1_fun1; base = obj1_cyc1; args = obj1_simp1; call_type = Method };
  CFunEnter { f = obj1_fun1; this = obj1_cyc1; args = obj1_simp1 };
  CDeclare { name = "arguments"; value = obj1_simp1; declaration_type = ArgumentArray };
  CDeclare { name = "x"; value = vundef; declaration_type = ArgumentBinding 0 };
  CReturn vfalse;

  CFunExit { ret = vfalse; exc = vundef };
  CFunPost { f = obj1_fun1; args = obj1_simp1; call_type = Method; result = vfalse; base = obj1_cyc1 };
  CScriptEnter;
  CBinary { op = "+"; left = v0; right = v1; result = v1 };
  CUnary { op = "-"; arg = v0; result = v0 };

  CScriptExit;
  CGetField { base = obj1_simp1; offset = "marker"; value = vundef };
  CPutField { base = obj1_simp1; offset = "marker"; value = vundef };
  CLiteral { value = obj1_simp2; hasGetterSetter = false };
  CDeclare { name = "y"; value = obj1_simp2; declaration_type = Var };

  CConditional vfalse
]

let cleantrace1_args = [
  None;
  None;
  None;
  None;
  None;

  None;
  None;
  None;
  None;
  None;

  None;
  Some (get_object obj1_simp1);
  Some (get_object obj1_simp1);
  Some (get_object obj1_simp1);
  Some (get_object obj1_simp1);

  None;
  None;
  None;
  None;
  None;

  None;
  None;
  None;
  None;
  None;

  None
]

let ct1_l_e = Reference.reference_of_local_name "e"
and ct1_g_x = Reference.reference_of_name true StringMap.empty true "x"
and ct1_args = Reference.reference_of_local_name "arguments"
and ct1_arg0 = Reference.reference_of_field obj1_simp1 "0"
and ct1_arg1 = Reference.reference_of_field obj1_simp1 "1"
and ct1_toString = Reference.reference_of_field obj1_simp1 "toString"
and ct1_toString' = Reference.reference_of_field obj1_simp2 "toString"
and ct1_l_y = Reference.reference_of_local_name "y"
and ct1_f_simp1_marker = Reference.reference_of_field obj1_simp1 "marker"
and ct1_this = Reference.reference_of_local_name "this"

let cleantrace1_updates = 
  let init = Reference.reference_of_fieldref (Function (0, 2), "toString") in [
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);

    Some (ct1_l_e, 0);
    Some (ct1_l_e, 0);
    Some (ct1_l_e, 0);
    Some (ct1_g_x, 0);
    Some (ct1_g_x, 0);

    Some (ct1_toString, 0);
    Some (ct1_this, 0);
    Some (ct1_args, 0);
    Some (ct1_args, 0);
    Some (ct1_args, 0);

    Some (ct1_args, 0);
    Some (ct1_args, 0);
    Some (ct1_args, 0);
    Some (ct1_args, 0);
    Some (ct1_args, 0);

    Some (ct1_args, 0);
    Some (ct1_f_simp1_marker, 0);
    Some (ct1_f_simp1_marker, 0);
    Some (ct1_toString', 0);
    Some (ct1_l_y, 0);

    Some (ct1_l_y, 0)
  ]

let ct1ver_init =
  let add_field obj fld map =
    Reference.ReferenceMap.add (Reference.reference_of_fieldref (obj, fld)) 0 map
  in let add_basic obj map =
       map
       |> add_field obj "Function"
       |> add_field obj "apply"
       |> add_field obj "call"
       |> add_field obj "prototype"
       |> add_field obj "toString"
  in
  Reference.ReferenceMap.empty
  |> add_basic (Object 0)
  |> add_basic (Function (0,0))
  |> add_basic (Function (0,1))
  |> add_basic (Function (0,2))

let ct1ver_e = Reference.ReferenceMap.add ct1_l_e 0 ct1ver_init
let ct1ver_x = Reference.ReferenceMap.add ct1_g_x 0 ct1ver_e
let ct1ver_funpre = let open Reference.ReferenceMap in
  ct1ver_x |> add ct1_arg0 0 |> add ct1_arg1 0 |> add ct1_toString 0
let ct1ver_enter = let open Reference.ReferenceMap in
  ct1ver_funpre |> add ct1_this 0
let ct1ver_args =  let open Reference.ReferenceMap in
  ct1ver_enter |> add ct1_args 0
let ct1ver_arg0 = Reference.ReferenceMap.add ct1_arg0 0 ct1ver_args
let ct1ver_f = Reference.ReferenceMap.add ct1_f_simp1_marker 0 ct1ver_funpre
let ct1ver_f' = Reference.ReferenceMap.add ct1_f_simp1_marker 1 ct1ver_f
let ct1ver_simp2 = Reference.ReferenceMap.add ct1_toString' 0 ct1ver_f'
let ct1ver_y = Reference.ReferenceMap.add ct1_l_y 0 ct1ver_simp2

let cleantrace1_versions =
  [
    ct1ver_init;
    ct1ver_init;
    ct1ver_init;
    ct1ver_init;
    ct1ver_init;

    ct1ver_e;
    ct1ver_e;
    ct1ver_e;
    ct1ver_x;
    ct1ver_x;

    ct1ver_funpre;
    ct1ver_enter;
    ct1ver_args;
    ct1ver_arg0;
    ct1ver_arg0;

    ct1ver_funpre;
    ct1ver_funpre;
    ct1ver_funpre;
    ct1ver_funpre;
    ct1ver_funpre;

    ct1ver_funpre;
    ct1ver_f;
    ct1ver_f';
    ct1ver_simp2;
    ct1ver_y;

    ct1ver_y
  ]

let ct1al_emp = StringMap.empty
let ct1al_x = StringMap.add "x" (objectid_of_jsval obj1_simp1, "0") ct1al_emp

let cleantrace1_aliases = [
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;

  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;

  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_x;
  ct1al_x;

  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;

  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;
  ct1al_emp;

  ct1al_emp
]

let rec combine_facts args updates versions aliases pointsto =
  match args, updates, versions, aliases, pointsto with
    | last_arguments::args, last_update::updates, versions::versions', aliases::aliases', points_to::points_to' ->
        { last_arguments; last_update; versions; aliases; points_to } ::
        combine_facts args updates versions' aliases' points_to'
    | [], [], [], [], [] -> []
    | _ -> invalid_arg "Lengths do not match"

module VRM = Reference.VersionReferenceMap
let add_simple_fields (obj: objectid) (fields: (string * jsval) list) ptm =
  List.fold_left
    (fun ptm (fld, value) -> VRM.add (Reference.reference_of_fieldref (obj, fld), 0) value ptm)
    ptm fields
let add_func obj ptm =
  add_simple_fields obj
    [ ("Function", OObject 0); ("apply", OFunction(0,0)); ("call", OFunction(0,1)); ("prototype", OObject 0); ("toString", OFunction(0,2)) ]
    ptm

let ref_this = Reference.reference_of_local_name "this"

let trace1_ptinit =
  VRM.empty
    |> add_func (Function (0, 0))
    |> add_func (Function (0, 1))
    |> add_func (Function (0, 2))
    |> add_func (Object 0)
    |> VRM.add (ref_this, 0) (OObject 0)

let trace1_pte = VRM.add (ct1_l_e, 0) obj1_simp2 trace1_ptinit
let trace1_ptx = VRM.add (ct1_g_x, 0) vtrue trace1_pte
let trace1_pto11_1 = add_simple_fields (Object 11) [ "0", vnull; "1", vundef; "toString", (OFunction (0, 2)) ] trace1_ptx
let trace1_pt_this = (*VRM.add (ref_this, 1) obj1_cyc1 *) trace1_pto11_1 (* TODO why is this not added??? *)
let trace1_pt_arguments = VRM.add (ct1_args, 0) obj1_simp1 trace1_pt_this
let trace1_pt_arg0 = VRM.add (ct1_arg0, 0) vnull trace1_pt_arguments
let trace1_pto11_1_marker = add_simple_fields (Object 11) [ "marker", vundef ] trace1_pt_arg0
let trace1_pto11_1_marker' = VRM.add (Reference.reference_of_fieldref (Object 11, "marker"), 1) vundef trace1_pto11_1_marker
let trace1_pto12 = add_simple_fields (Object 12) [ "toString", (OFunction (0, 2)) ] trace1_pto11_1_marker'
let trace1_pty = VRM.add (ct1_l_y, 0) obj1_simp2 trace1_pto12

let trace1_pointsto = trace1_pty

let cleantrace1_pointsto =
  [
    trace1_ptinit;
    trace1_ptinit;
    trace1_ptinit;
    trace1_ptinit;
    trace1_ptinit;

    trace1_pte;
    trace1_pte;
    trace1_pte;
    trace1_ptx;
    trace1_ptx;

    trace1_pto11_1;
    trace1_pt_this;
    trace1_pt_arguments;
    trace1_pt_arg0;
    trace1_pt_arg0;

    trace1_pt_arg0;
    trace1_pt_arg0;
    trace1_pt_arg0;
    trace1_pt_arg0;
    trace1_pt_arg0;

    trace1_pt_arg0;
    trace1_pto11_1_marker;
    trace1_pto11_1_marker';
    trace1_pto12;
    trace1_pty;

    trace1_pty
  ]
 
let cleantrace1_facts =
  combine_facts cleantrace1_args cleantrace1_updates cleantrace1_versions cleantrace1_aliases cleantrace1_pointsto

let argtrace1 = List.combine cleantrace1 cleantrace1_args
let facttrace1 = List.combine cleantrace1 cleantrace1_facts

let facts_init = { last_arguments = None; last_update = None; versions = ct1ver_init; aliases = ct1al_emp; points_to = trace1_ptinit }
let facts_catch = { last_arguments = None; last_update = Some (ct1_l_e, 0); versions = ct1ver_e; aliases = ct1al_emp ; points_to = trace1_pte }
let facts_write1 = { last_arguments = None; last_update = Some (ct1_g_x, 0); versions = ct1ver_x; aliases = ct1al_emp ; points_to = trace1_ptx }
let facts_funpre = { last_arguments = None; last_update = Some (ct1_args, 0); versions = ct1ver_funpre; aliases = ct1al_emp; points_to = trace1_pto11_1 }
let facts_funenter = { last_arguments = Some (get_object obj1_simp1); last_update = Some (ct1_args, 0); versions = ct1ver_enter; aliases = ct1al_emp; points_to = trace1_pt_this }
let facts_arguments = { last_arguments = Some (get_object obj1_simp1); last_update = Some (ct1_args, 0); versions = ct1ver_args; aliases = ct1al_emp; points_to = trace1_pt_arguments }
let facts_arg0 = { last_arguments = Some (get_object obj1_simp1); last_update = Some (ct1_args, 0); versions = ct1ver_arg0; aliases = ct1al_x; points_to = trace1_pt_arg0 }
let facts_exit = { last_arguments = None; last_update = Some (ct1_args, 0); versions = ct1ver_funpre; aliases = ct1al_emp; points_to = trace1_pt_arg0 }
let facts_post = { last_arguments = None; last_update = Some (ct1_args, 0); versions = ct1ver_funpre; aliases = ct1al_emp; points_to = trace1_pt_arg0 }
let facts_write2 = { last_arguments = None; last_update = Some (ct1_f_simp1_marker, 0); versions = ct1ver_f; aliases = ct1al_emp; points_to = trace1_pto11_1_marker }
let facts_write3 = { last_arguments = None; last_update = Some (ct1_f_simp1_marker, 0); versions = ct1ver_f'; aliases = ct1al_emp; points_to = trace1_pto11_1_marker' }
let facts_literal = { last_arguments = None; last_update = Some (ct1_f_simp1_marker, 0); versions = ct1ver_simp2; aliases = ct1al_emp; points_to = trace1_pto12 }
let facts_local = { last_arguments = None; last_update = Some (ct1_l_y, 0); versions = ct1ver_y; aliases = ct1al_emp; points_to = trace1_pty }


let richtrace1 = [
    RForIn obj1_simp2, facts_init;
    RWith obj1_simp2, facts_init; (* FIXME handle with properly *)
    RScriptEnter, facts_init;
    RThrow obj1_simp2, facts_init;
    RScriptExc obj1_simp2, facts_init;

    RCatch { name = "e"; ref = (ct1_l_e, 0) }, facts_catch;
    RWrite { ref = (ct1_l_e, 0); oldref = (ct1_l_e, 0); value = obj1_simp2; success = true }, facts_catch;
    REndExpression, facts_catch;
    RLiteral { value = vtrue; hasGetterSetter = false }, facts_catch;
    RWrite { ref = (ct1_g_x, 0); oldref = (ct1_g_x, 0); value = vtrue; success = true }, facts_write1;

    RRead { ref = (ct1_g_x, 0); value = vtrue }, facts_write1;
    RFunPre { f = obj1_fun1; base = obj1_cyc1; args = obj1_simp1; call_type = Method }, facts_funpre;
    RFunEnter { f = obj1_fun1; this = obj1_cyc1; args = obj1_simp1 }, facts_funenter;
    RLocal { name = "arguments"; ref = (ct1_args, 0) }, facts_arguments;
    RWrite { ref = (ct1_args, 0); oldref = (ct1_args, 0); value = obj1_simp1; success = true }, facts_arguments;

    RAlias { name = "x"; ref = (ct1_arg0, 0); source = Argument 0 }, facts_arg0;
    RReturn vfalse, facts_arg0;
    RFunExit { ret = vfalse; exc = vundef }, facts_exit;
    RFunPost { f = obj1_fun1; args = obj1_simp1; result = vfalse; base = obj1_cyc1; call_type = Method }, facts_post;
    RScriptEnter, facts_post;

    RBinary { op = "+"; left = v0; right = v1; result = v1 }, facts_post;
    RUnary { op = "-"; arg = v0; result = v0 }, facts_post;
    RScriptExit, facts_post;
    RRead { ref = (ct1_f_simp1_marker, 0); value = vundef }, facts_write2;
    RWrite { ref = (ct1_f_simp1_marker, 1); oldref = (ct1_f_simp1_marker, 0); value = vundef; success = true }, facts_write3;

    RLiteral { value = obj1_simp2; hasGetterSetter = false }, facts_literal;
    RLocal { name = "y"; ref = (ct1_l_y, 0) }, facts_local;
    RWrite { ref = (ct1_l_y, 0); oldref = (ct1_l_y, 0); value = obj1_simp2; success = true }, facts_local;
    RConditional vfalse, facts_local;
  ]


let ct2_l_e = Reference.reference_of_local_name "e"
and ct2_g_x = Reference.reference_of_name true StringMap.empty true "x"
and ct2_args = Reference.reference_of_local_name "arguments"
and ct2_arg0 = Reference.reference_of_field obj2_simp1 "0"
and ct2_arg1 = Reference.reference_of_field obj2_simp1 "1"
and ct2_toString = Reference.reference_of_field obj2_simp1 "toString"
and ct2_toString' = Reference.reference_of_field obj2_simp2 "toString"
and ct2_l_y = Reference.reference_of_local_name "y"
and ct2_f_simp1_marker = Reference.reference_of_field obj1_simp1 "marker"
and ct2_this = Reference.reference_of_local_name "this"

let ct2ver_init =
  let add_field obj fld map =
    Reference.ReferenceMap.add (Reference.reference_of_fieldref (obj, fld)) 0 map
  in let add_basic obj map =
       map
       |> add_field obj "Function"
       |> add_field obj "apply"
       |> add_field obj "call"
       |> add_field obj "prototype"
       |> add_field obj "toString"
  in
  Reference.ReferenceMap.empty
  |> add_basic (Object 0)
  |> add_basic (Function (0,0))
  |> add_basic (Function (0,1))
  |> add_basic (Function (0,2))

let ct2ver_e = Reference.ReferenceMap.add ct2_l_e 0 ct2ver_init
let ct2ver_x = Reference.ReferenceMap.add ct2_g_x 0 ct2ver_e
let ct2ver_funpre = let open Reference.ReferenceMap in
  ct2ver_x |> add ct2_arg0 0 |> add ct2_arg1 0 |> add ct2_toString 0
let ct2ver_enter = let open Reference.ReferenceMap in
  ct2ver_funpre |> add ct2_this 0
let ct2ver_args =  let open Reference.ReferenceMap in
  ct2ver_enter |> add ct2_args 0
let ct2ver_arg0 = Reference.ReferenceMap.add ct2_arg0 0 ct2ver_args
let ct2ver_f = Reference.ReferenceMap.add ct2_f_simp1_marker 0 ct2ver_funpre
let ct2ver_f' = Reference.ReferenceMap.add ct2_f_simp1_marker 1 ct2ver_f
let ct2ver_simp2 = Reference.ReferenceMap.add ct2_toString' 0 ct2ver_f'
let ct2ver_y = Reference.ReferenceMap.add ct2_l_y 0 ct2ver_simp2

let cleantrace2_versions =
  [
    ct2ver_init;
    ct2ver_init;
    ct2ver_init;
    ct2ver_init;
    ct2ver_init;

    ct2ver_e;
    ct2ver_e;
    ct2ver_e;
    ct2ver_x;
    ct2ver_x;

    ct2ver_funpre;
    ct2ver_enter;
    ct2ver_args;
    ct2ver_arg0;
    ct2ver_arg0;

    ct2ver_funpre;
    ct2ver_funpre;
    ct2ver_funpre;
    ct2ver_funpre;
    ct2ver_funpre;

    ct2ver_funpre;
    ct2ver_f;
    ct2ver_f';
    ct2ver_simp2;
    ct2ver_y;

    ct2ver_y
  ]

let ct2al_emp = StringMap.empty
let ct2al_x = StringMap.add "x" (objectid_of_jsval obj2_simp1, "0") ct2al_emp

let cleantrace2_aliases = [
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;

  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;

  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_x;
  ct2al_x;

  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;

  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;
  ct2al_emp;

  ct2al_emp
]

let cleantrace2_args = [
  None;
  None;
  None;
  None;
  None;

  None;
  None;
  None;
  None;
  None;

  None;
  Some (get_object obj2_simp1);
  Some (get_object obj2_simp1);
  Some (get_object obj2_simp1);
  Some (get_object obj2_simp1);

  None;
  None;
  None;
  None;
  None;

  None;
  None;
  None;
  None;
  None;

  None
]

let cleantrace2_updates = 
  let init = Reference.reference_of_fieldref (Function (0, 2), "toString") in [
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);

    Some (ct2_l_e, 0);
    Some (ct2_l_e, 0);
    Some (ct2_l_e, 0);
    Some (ct2_g_x, 0);
    Some (ct2_g_x, 0);

    Some (ct2_g_x, 0);
    Some (ct2_g_x, 0);
    Some (ct2_args, 0);
    Some (ct2_arg0, 0);
    Some (ct2_arg0, 0);

    Some (ct2_arg0, 0);
    Some (ct2_arg0, 0);
    Some (ct2_arg0, 0);
    Some (ct2_arg0, 0);
    Some (ct2_arg0, 0);

    Some (ct2_arg0, 0);
    Some (ct2_arg0, 0);
    Some (ct2_f_simp1_marker, 1);
    Some (ct2_f_simp1_marker, 1);
    Some (ct2_l_y, 0);

    Some (ct2_l_y, 0)
  ]

let trace2_ptinit =
  VRM.empty
    |> add_func (Function (0, 0))
    |> add_func (Function (0, 1))
    |> add_func (Function (0, 2))
    |> add_func (Object 0)
    |> VRM.add (ref_this, 0) (OObject 0)

let trace2_pte = VRM.add (ct2_l_e, 0) obj1_simp2 trace1_ptinit
let trace2_ptx = VRM.add (ct2_g_x, 0) vtrue trace1_pte
let trace2_pto11_1 = add_simple_fields (Object 12) [ "0", vnull; "1", vundef; "toString", (OFunction (0, 2)) ] trace1_ptx
let trace2_pt_this = (*VRM.add (ref_this, 1) obj1_cyc1*) trace1_pto11_1 (* TODO why is this not added??? *)
let trace2_pt_arguments = VRM.add (ct2_args, 0) obj1_simp1 trace2_pt_this
let trace2_pt_arg0 = VRM.add (ct2_arg0, 0) vnull trace2_pt_arguments
let trace2_pto11_1_marker = add_simple_fields (Object 12) [ "marker", vundef ] trace2_pt_arg0
let trace2_pto11_1_marker' = VRM.add (Reference.reference_of_fieldref (Object 11, "marker"), 1) vundef trace2_pto11_1_marker
let trace2_pto12 = add_simple_fields (Object 13) [ "toString", (OFunction (0, 2)) ] trace2_pto11_1_marker'
let trace2_pty = VRM.add (ct2_l_y, 0) obj1_simp2 trace2_pto12

let trace2_pointsto = trace2_pty

let cleantrace2_pointsto =
  [
    trace2_ptinit;
    trace2_ptinit;
    trace2_ptinit;
    trace2_ptinit;
    trace2_ptinit;

    trace2_pte;
    trace2_pte;
    trace2_pte;
    trace2_ptx;
    trace2_ptx;

    trace2_pto11_1;
    trace2_pt_this;
    trace2_pt_arguments;
    trace2_pt_arg0;
    trace2_pt_arg0;

    trace2_pt_arg0;
    trace2_pt_arg0;
    trace2_pt_arg0;
    trace2_pt_arg0;
    trace2_pt_arg0;

    trace2_pt_arg0;
    trace2_pto11_1_marker;
    trace2_pto11_1_marker';
    trace2_pto12;
    trace2_pty;

    trace2_pty
  ]
let cleantrace2_facts =
  combine_facts cleantrace2_args cleantrace2_updates cleantrace2_versions cleantrace2_aliases cleantrace2_pointsto

let (|?) x d = match x with Some x -> x | None -> d

let assert_is_None ?prn ?msg =
  function
  | Some x ->
    Kaputt.Assertion.fail "None"
      (match prn with Some f -> "Some " ^ f x | None -> "Some ...")
      (msg |? "Values incompatible") 
  | None -> ()

let assert_is_Some ?prn ?msg =
  function
  | Some _ -> ()
  | None -> Kaputt.Assertion.fail "Some ..." "None" (msg |? "Values incompatible")

let test_lf1 = local_facts_1
let test_lf2 = local_facts_2

let test_rt1 = { 
    funcs = functab1;
    objs = objtab1;
    trace = [];
    globals = globals;
    globals_are_properties = true;
    points_to = points_to_1
  }

let test_rt2 = { 
    funcs = functab2;
    objs = objtab2;
    trace = [];
    globals = globals;
    globals_are_properties = true;
    points_to = points_to_2
  }

module AssertStringMap =
  Kaputt.Assertion.Map(StringMap)(struct type t = string let to_string x = x end)
module AssertReferenceMap =
  Kaputt.Assertion.Map(Reference.ReferenceMap)
    (struct
       type t = Reference.reference
       let to_string = (Fmt.to_to_string Reference.pp_reference)
     end)
module AssertVersionedReferenceMap = 
  Kaputt.Assertion.Map(Reference.VersionReferenceMap)
    (struct
       type t = Reference.versioned_reference
       let to_string = (Fmt.to_to_string Reference.pp_versioned_reference)
     end)

let make_equal_extarray ~msg eq prn x y =
  Kaputt.Assertion.make_equal_array ~msg:msg eq prn (BatDynArray.to_array x) (BatDynArray.to_array y)
let same_jsval = Kaputt.Assertion.make_equal (=) (Fmt.to_to_string pp_jsval)
let same_fieldspec = Kaputt.Assertion.make_equal (=) (Fmt.to_to_string pp_fieldspec)
let same_objectspec = AssertStringMap.make_equal (=) (Fmt.to_to_string pp_fieldspec)
let same_objects = make_equal_extarray ~msg:"objects"
                     (StringMap.equal (=)) (Fmt.to_to_string pp_objectspec)
let same_funcspec = Kaputt.Assertion.make_equal (=) (Fmt.to_to_string pp_funcspec)
let same_functions = make_equal_extarray ~msg:"functions" (=) (Fmt.to_to_string pp_funcspec)
let same_globals = AssertStringMap.make_equal (=) (Fmt.to_to_string pp_jsval)
let same_objectid = Kaputt.Assertion.make_equal (=) (Fmt.to_to_string pp_objectid)
let same_fieldref = Kaputt.Assertion.make_equal (=) (Fmt.to_to_string pp_fieldref)
let same_event = Kaputt.Assertion.make_equal (=) (Fmt.to_to_string pp_operation)
let same_trace = Kaputt.Assertion.make_equal_list (=) (Fmt.to_to_string pp_operation)
let same_tracefile (f1, o1, t1, g1, p1) (f2, o2, t2, g2, p2) =
  same_functions f1 f2;
  same_objects o1 o2;
  same_trace t1 t2;
  same_globals g1 g2;
  Kaputt.Assertion.equal_bool ~msg:"globals are properties" p1 p2
let same_clean_operation = Kaputt.Assertion.make_equal (=) (Fmt.to_to_string pp_clean_operation)
let same_clean_trace = Kaputt.Assertion.make_equal_list (=) (Fmt.to_to_string pp_clean_operation)
let same_clean_tracefile (f1, o1, t1, g1, p1) (f2, o2, t2, g2, p2) =
  same_functions f1 f2;
  same_objects o1 o2;
  same_clean_trace t1 t2;
  same_globals g1 g2;
  Kaputt.Assertion.equal_bool ~msg:"globals are properties" p1 p2
let same_local_facts f1 f2 =
  Kaputt.Assertion.make_equal ~msg:"last arguments don't match" (=)
    (Fmt.to_to_string (Fmt.option Fmt.int))
    f1.last_arguments f2.last_arguments;
  Kaputt.Assertion.make_equal ~msg:"last updates don't match" (=)
    (Fmt.to_to_string (Fmt.option Reference.pp_versioned_reference))
    f1.last_update f2.last_update;
  AssertReferenceMap.make_equal (=) string_of_int f1.versions f2.versions;
  AssertStringMap.make_equal (=) (Fmt.to_to_string pp_fieldref) f1.aliases f2.aliases

let same_enriched_trace eq_a pp_a tr1 tr2 =
  Kaputt.Assertion.make_equal_list (fun (op1, en1) (op2, en2) -> op1 = op2 && eq_a en1 en2)
    (Fmt.to_to_string (Fmt.pair pp_clean_operation pp_a)) tr1 tr2
let same_enriched_trace_file eq_a pp_a (f1, o1, t1, g1, p1) (f2, o2, t2, g2, p2) =
  same_functions f1 f2;
  same_objects o1 o2;
  same_enriched_trace eq_a pp_a t1 t2;
  same_globals g1 g2;
  Kaputt.Assertion.equal_bool ~msg:"globals are properties" p1 p2

let equal_local_facts f1 f2 =
  f1.last_arguments = f2.last_arguments &&
  f1.last_update = f2.last_update &&
  Reference.ReferenceMap.equal (=) f1.versions f2.versions &&
  StringMap.equal (=) f1.aliases f2.aliases

let same_facts_trace = same_enriched_trace equal_local_facts pp_local_facts
let same_facts_tracefile = same_enriched_trace_file equal_local_facts pp_local_facts
let same_arguments_trace = same_enriched_trace (=) (Fmt.option Fmt.int)
let same_arguments_tracefile = same_enriched_trace_file (=) (Fmt.option Fmt.int)

let same_rich_trace =
  Kaputt.Assertion.make_equal_list
    (fun (o1, f1) (o2, f2) -> o1 = o2 && equal_local_facts f1 f2)
    (Fmt.to_to_string (Fmt.pair pp_rich_operation pp_local_facts))
let same_rich_tracefile t1 t2 =
  same_functions t1.funcs t2.funcs;
  same_objects t1.objs t2.objs;
  same_rich_trace t1.trace t2.trace;
  same_globals t1.globals t2.globals;
  Kaputt.Assertion.equal_bool ~msg:"globals are properties"
    t1.globals_are_properties t2.globals_are_properties;
  AssertVersionedReferenceMap.make_equal (=) (Fmt.to_to_string pp_jsval)
    t1.points_to t2.points_to
