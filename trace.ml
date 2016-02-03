open Types
open TraceTypes

open Yojson.Basic;;
open Yojson.Basic.Util;;

exception ParseError
let report err msg json = Format.eprintf "%s - %s: %s@." err msg (Yojson.Basic.to_string json); raise ParseError

let to_string err json =
  try to_string json with Type_error (msg, json) -> report err msg json

let parse_jsval json =
  try
    match member "type" json |> to_string "Value type" with
    | "undefined" -> OUndefined
    | "boolean" -> OBoolean (member "val" json |> to_string "Value" |> bool_of_string)
    | "number" ->
      let numstr = member "val" json |> to_string "Value" in begin
        try ONumberInt (int_of_string numstr)
        with Failure "int_of_string" ->
          try ONumberFloat (float_of_string numstr)
          with Failure "float_of_string" ->
            failwith ("Strange number here: " ^ Yojson.Basic.to_string json)
      end
    | "string" -> OString (member "val" json |> to_string "Value")
    | "symbol" -> OSymbol (member "val" json |> to_string "Value")
    | "function" -> OFunction (member "id" json |> to_int, member "funid" json |> to_int)
    | "null" -> ONull
    | "object" -> OObject (member "id" json |> to_int)
    | _ as ty -> OOther (ty, member "id" json |> to_int)
  with
  | ParseError -> report "Context" "jsval" json

let native_pattern = Str.regexp_string "[native code]"
let parse_funcspec json =
  let instr = member "instrumented" json |> to_string "Function specification" in
  if (Str.string_match native_pattern instr 0)
  then External (json |> member "obj" |> to_int)
  else Local { from_toString = instr; from_jalangi = json |> member "uninstrumented" |> to_string_option }

let parse_fieldspec json =
  let default_to d = function Some x -> x | None -> d in try
    { value = json |> member "value" |> to_option parse_jsval |> default_to OUndefined;
      writable = json |> member "writable" |> to_bool_option |> default_to true;
      enumerable = json |> member "enumerable" |> to_bool_option |> default_to true;
      configurable = json |> member "configurable" |> to_bool_option |> default_to true;
      get = json |> member "get" |> to_option parse_jsval;
      set = json |> member "set" |> to_option parse_jsval
    }
  with ParseError -> report "Context" "fieldspec" json


let parse_objectspec json =
  try
    json |> to_assoc |>
    List.fold_left
      (fun spec (name, content) ->
         StringMap.add name (parse_fieldspec content) spec)
      StringMap.empty
  with ParseError -> report "Context" "objectspec" json

let parse_operation json =
  let get_int key = member key json |> to_int
  and get_string key = member key json |> to_string key
  and get_jsval key = try member key json |> parse_jsval with e -> Format.eprintf "Can't find jsval %s@." key; raise e
  and get_bool key = try member key json |> to_bool with e -> Format.eprintf "Can't find bool %s@." key; raise e in
  try
    match member "step" json |> to_string "Step" with
    | "funpre" -> FunPre (get_int "iid", {
        f = get_jsval "f";
        base = get_jsval "base";
        args = get_jsval "args";
        isConstructor = get_bool "isConstructor";
        isMethod = get_bool "isMethod"
      })
    | "funpost" -> FunPost (get_int "iid", {
        f = get_jsval "f";
        base = get_jsval "base";
        args = get_jsval "args";
        isConstructor = get_bool "isConstructor";
        isMethod = get_bool "isMethod";
        result = get_jsval "result"
      })
    | "literal" -> Literal (get_int "iid", {
        value = get_jsval "val";
        hasGetterSetter = get_bool "hasGetterSetter"
      })
    | "forin" -> ForIn (get_int "iid", get_jsval "val")
    | "declare" -> Declare (get_int "iid", {
        name = get_string "name";
        value = get_jsval "val";
        isCatchParam = get_bool "isCatchParam";
        argument = if get_bool "isArgument"
          then Some (get_int "argumentIndex") else None
      })
    | "getpre" -> GetFieldPre (get_int "iid", {
        base = get_jsval "base";
        offset = get_string "offset";
        isComputed = get_bool "isComputed";
        isOpAssign = get_bool "isOpAssign";
        isMethodCall = get_bool "isMethodCall"
      })
    | "getpost" -> GetField (get_int "iid", {
        base = get_jsval "base";
        offset = get_string "offset";
        isComputed = get_bool "isComputed";
        isOpAssign = get_bool "isOpAssign";
        isMethodCall = get_bool "isMethodCall";
        value = get_jsval "val"
      })
    | "putpre" -> PutFieldPre (get_int "iid", {
        base = get_jsval "base";
        offset = get_string "offset";
        isComputed = get_bool "isComputed";
        isOpAssign = get_bool "isOpAssign";
        value = get_jsval "val"
      })
    | "putpost" -> PutField (get_int "iid", {
        base = get_jsval "base";
        offset = get_string "offset";
        isComputed = get_bool "isComputed";
        isOpAssign = get_bool "isOpAssign";
        value = get_jsval "val"
      })
    | "read" -> Read (get_int "iid", {
        name = get_string "name";
        value = get_jsval "val";
        isGlobal = get_bool "isGlobal";
        isScriptLocal = get_bool "isScriptLocal"
      })
    | "write" -> Write (get_int "iid", {
        name = get_string "name";
        value = get_jsval "val";
        lhs = get_jsval "lhs";
        isGlobal = get_bool "isGlobal";
        isScriptLocal = get_bool "isScriptLocal"
      })
    | "return" -> Return (get_int "iid", get_jsval "val")
    | "throw" -> Throw (get_int "iid", get_jsval "val")
    | "with" -> With (get_int "iid", get_jsval "val")
    | "funcenter" -> FunEnter (get_int "iid", {
        f = get_jsval "f";
        this = get_jsval "this";
        args = get_jsval "args"
      })
    | "funcexit" -> FunExit (get_int "iid", {
        ret = get_jsval "ret";
        exc = get_jsval "exc"
      })
    | "scriptenter" -> ScriptEnter
    | "scriptexit" -> ScriptExit
    | "scriptexc" -> ScriptExc (get_jsval "exc")
    | "binarypre" -> BinPre (get_int "iid", {
        op = get_string "op";
        left = get_jsval "left";
        right = get_jsval "right";
        isOpAssign = get_bool "isOpAssign";
        isSwitchCaseComparison =
          get_bool "isSwitchComparison";
        isComputed = get_bool "isComputed"
      })
    | "binarypost" -> BinPost (get_int "iid", {
        op = get_string "op";
        left = get_jsval "left";
        right = get_jsval "right";
        isOpAssign = get_bool "isOpAssign";
        isSwitchCaseComparison =
          get_bool "isSwitchComparison";
        isComputed = get_bool "isComputed";
        result = get_jsval "result"
      })
    | "unarypre" -> UnaryPre (get_int "iid", {
        op = get_string "op";
        arg = get_jsval "left"
      })
    | "unarypost" -> UnaryPost (get_int "iid", {
        op = get_string "op";
        arg = get_jsval "left";
        result = get_jsval "result"
      })
    | "exprend" -> EndExpression (get_int "iid")
    | "conditional" -> Conditional (get_jsval "result")
    | _ as op -> failwith ("Unknown event " ^ op)
  with ParseError -> report "Context" "event" json

let parse_functions json =
  try json |> convert_each parse_funcspec |> ExtArray.of_list
  with ParseError -> report "Context" "functions" json

let parse_objects json =
  try json |> convert_each parse_objectspec |> ExtArray.of_list
  with ParseError -> report "Context" "objects" json

let parse_trace json =
  try json |> convert_each parse_operation
  with ParseError -> report "Context" "trace" json

let parse_global_value json = (* Backwards compatibility! *)
  match member "id" json with
  | `Null -> parse_jsval json
  | `Int _ -> parse_jsval json
  | json' -> parse_jsval json'


let parse_globals json: globals =
  try
    json |> to_assoc |> StringMap.of_list |>
    StringMap.map parse_global_value
  with ParseError -> report "Context" "globals" json

let parse_tracefile source =
  let json = from_channel source in
  (parse_functions (member "func" json),
   parse_objects (member "obj" json),
   parse_trace (member "trace" json),
   parse_globals (member "globals" json),
   to_bool (member "globals_are_properties" json))

let event_of_string str = from_string str |> parse_operation
let jsval_of_string str = from_string str |> parse_jsval
let objectspec_of_string str = from_string str |> parse_objectspec
let funcspec_of_string str = from_string str |> parse_funcspec

(*******)
(*val format_tracefile : out_channel -> tracefile -> unit*)

let format_jsval = function
  | OUndefined -> `Assoc ["type", `String "undefined" ]
  | OBoolean b -> `Assoc [("type", `String "boolean"); ("val", `String (string_of_bool b))]
  | ONumberInt n -> `Assoc [("type", `String "number"); ("val", `String (string_of_int n))]
  | ONumberFloat n ->
      `Assoc [("type", `String "number"); ("val", `String (string_of_float n))]
  | OString s -> `Assoc [("type", `String "string"); ("val", `String s)]
  | OSymbol s -> `Assoc [("type", `String "symbol"); ("val", `String s)]
  | OFunction (id, funid) ->
      `Assoc [("type", `String "function"); ("id", `Int id); ("funid", `Int funid)]
  | ONull -> `Assoc ["type", `String "null" ]
  | OObject id -> `Assoc [("type", `String "object"); ("id", `Int id)]
  | OOther (ty, id) -> `Assoc [("type", `String ty); ("id", `Int id)]

let string_of_jsval v = format_jsval v |> Yojson.Basic.to_string

let format_fieldspec { value; writable; get; set; enumerable; configurable } =
  let fs =
    [ ("writable", `Bool writable); ("enumerable", `Bool enumerable);
      ("configurable", `Bool configurable) ] in
  let fs = match get with
    | Some getter -> ("get", format_jsval getter) :: fs
    | None -> ("value", format_jsval value) :: fs in
  let fs = match set with
    | Some setter -> ("set", format_jsval setter) :: fs
    | None -> fs
  in `Assoc fs

let format_objectspec objs =
  `Assoc (StringMap.fold (fun fld fs json -> (fld, format_fieldspec fs) :: json)
            objs [])

let string_of_objectspec objs = format_objectspec objs |> Yojson.Basic.to_string

let format_objects objs =
  `List (ExtArray.to_list objs |> List.map format_objectspec)

let format_funcspec = function
  | Local { from_toString; from_jalangi = Some from_jalangi } ->
      `Assoc [("instrumented", `String from_toString);
              ("uninstrumented", `String from_jalangi)]
  | Local { from_toString; from_jalangi = None } ->
      `Assoc [("instrumented", `String from_toString)]
  | External id ->
      `Assoc [("instrumented", `String "[native code]"); ("obj", `Int id)]

let string_of_funcspec funs = format_funcspec funs |> Yojson.Basic.to_string

let format_functions funs =
  `List (ExtArray.to_list funs |> List.map format_funcspec)

let format_globals globals =
  `Assoc (StringMap.fold (fun name obj glob -> (name, format_jsval obj) :: glob)
            globals [])

let format_objectid = function
  | Object id -> `Assoc [("type", `String "object"); ("id", `Int id)]
  | Function (id, funid) ->
      `Assoc [("type", `String "object"); ("id", `Int id); ("funid", `Int funid)]
  | Other (ty, id) -> `Assoc [("type", `String ty); ("id", `Int id)]

let format_fieldref (obj, fld) =
  `List [format_objectid obj; `String fld]

let format_event = function
  | FunPre (iid, { f; base; args; isConstructor; isMethod }) ->
      `Assoc [("step", `String "funpre"); ("iid", `Int iid); ("f", format_jsval f);
              ("base", format_jsval base); ("args", format_jsval args);
              ("isConstructor", `Bool isConstructor);
              ("isMethod", `Bool isMethod)]
  | FunPost (iid, { f; base; args; isConstructor; isMethod; result }) ->
      `Assoc [("step", `String "funpost"); ("iid", `Int iid); ("f", format_jsval f);
              ("base", format_jsval base); ("args", format_jsval args);
              ("isConstructor", `Bool isConstructor);
              ("isMethod", `Bool isMethod); ("result", format_jsval result)]
  | Literal (iid, { value; hasGetterSetter }) ->
      `Assoc [("step", `String "literal"); ("iid", `Int iid);
              ("val", format_jsval value); ("hasGetterSetter", `Bool hasGetterSetter)]
  | ForIn (iid, value) ->
      `Assoc [("step", `String "forin"); ("iid", `Int iid); ("val", format_jsval value)]
  | Declare (iid, { name; value; isCatchParam; argument }) ->
      begin match argument with
        | Some argidx ->
            `Assoc [("step", `String "declare"); ("iid", `Int iid); ("name", `String name);
                    ("val", format_jsval value);
                    ("isCatchParam", `Bool isCatchParam);
                    ("isArgument", `Bool true);
                    ("argumentIndex", `Int argidx)]
        | None ->
            `Assoc [("step", `String "declare"); ("iid", `Int iid); ("name", `String name);
                    ("val", format_jsval value);
                    ("isCatchParam", `Bool isCatchParam);
                    ("isArgument", `Bool false);
                    ("argumentIndex", `Int 0)]
      end
  | GetFieldPre (iid, {base; offset; isComputed; isOpAssign; isMethodCall}) ->
      `Assoc [("iid", `Int iid); ("base", format_jsval base); ("offset", `String offset);
              ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
              ("isMethodCall", `Bool isMethodCall);
              ("step", `String "getpre")]
  | GetField (iid, {base; offset; isComputed; isOpAssign; isMethodCall; value}) ->
      `Assoc [("iid", `Int iid); ("base", format_jsval base); ("offset", `String offset);
              ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
              ("isMethodCall", `Bool isMethodCall); ("val", format_jsval value);
              ("step", `String "getpost")]
  | PutFieldPre (iid, {base; offset; isComputed; isOpAssign; value}) ->
      `Assoc [("iid", `Int iid); ("base", format_jsval base); ("offset", `String offset);
              ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
              ("val", format_jsval value); ("step", `String "putpre") ]
  | PutField (iid, {base; offset; isComputed; isOpAssign; value}) ->
      `Assoc [("iid", `Int iid); ("base", format_jsval base); ("offset", `String offset);
              ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
              ("val", format_jsval value); ("step", `String "putpost") ]
  | Read (iid, {name; value; isGlobal; isScriptLocal}) ->
      `Assoc [("step", `String "read"); ("iid", `Int iid);
              ("name", `String name); ("val", format_jsval value);
              ("isScriptLocal", `Bool isScriptLocal); ("isGlobal", `Bool isGlobal)]
  | Write (iid, {name; value; lhs; isGlobal; isScriptLocal}) ->
      `Assoc [("step", `String "write"); ("iid", `Int iid);
              ("name", `String name); ("val", format_jsval value);
              ("lhs", format_jsval lhs);
              ("isScriptLocal", `Bool isScriptLocal); ("isGlobal", `Bool isGlobal)]
  | Return (iid, value) ->
      `Assoc [("step", `String "return"); ("iid", `Int iid); ("val", format_jsval value)]
  | Throw (iid, value) ->
      `Assoc [("step", `String "throw"); ("iid", `Int iid); ("val", format_jsval value)]
  | With (iid, value) ->
      `Assoc [("step", `String "with"); ("iid", `Int iid); ("val", format_jsval value)]
  | FunEnter (iid, { f; this; args }) ->
      `Assoc [("step", `String "funcenter"); ("iid", `Int iid);
              ("f", format_jsval f); ("this", format_jsval this);
              ("args", format_jsval args) ]
  | FunExit (iid, { ret; exc }) ->
      `Assoc [("step", `String "funcexit"); ("iid", `Int iid);
              ("ret", format_jsval ret); ("exc", format_jsval exc)]
  | ScriptEnter -> `Assoc [("step", `String "scriptenter")]
  | ScriptExit -> `Assoc [("step", `String "scriptexit")]
  | ScriptExc exc -> `Assoc [("step", `String "scriptexc"); ("exc", format_jsval exc)]
  | BinPre (iid, {op; left; right; isOpAssign; isSwitchCaseComparison; isComputed }) ->
      `Assoc [("step", `String "binarypre"); ("iid", `Int iid);
              ("op", `String op); ("left", format_jsval left);
              ("right", format_jsval right);
              ("isOpAssign", `Bool isOpAssign);
              ("isSwitchComparison", `Bool isSwitchCaseComparison);
              ("isComputed", `Bool isComputed) ]
  | BinPost (iid, {op; left; right; isOpAssign;
                   isSwitchCaseComparison; isComputed; result }) ->
      `Assoc [("step", `String "binarypost"); ("iid", `Int iid);
              ("op", `String op); ("left", format_jsval left);
              ("right", format_jsval right);
              ("isOpAssign", `Bool isOpAssign);
              ("isSwitchComparison", `Bool isSwitchCaseComparison);
              ("isComputed", `Bool isComputed); ("result", format_jsval result) ]
  | UnaryPre (iid, {op; arg}) ->
      `Assoc [("step", `String "unarypre"); ("iid", `Int iid);
              ("op", `String op); ("left", format_jsval arg)]
  | UnaryPost (iid, {op; arg; result}) ->
      `Assoc [("step", `String "unarypost"); ("iid", `Int iid);
              ("op", `String op); ("left", format_jsval arg);
              ("result", format_jsval result)]
  | EndExpression iid -> `Assoc [("step", `String "exprend"); ("iid", `Int iid)]
  | Conditional result -> `Assoc [("step", `String "conditional");
                                  ("result", format_jsval result)]

let string_of_event ev = format_event ev |> Yojson.Basic.to_string
let format_trace  tr = `List (List.map format_event tr)
let format_tracefile out (funs, objs, trace, globs, gap) =
  `Assoc [("func", format_functions funs);
          ("obj", format_objects objs);
          ("trace", format_trace trace);
          ("globals", format_globals globs);
          ("globals_are_properties", `Bool gap)]
    |> to_channel out

let format_call_type = function
  | Function -> `String "function"
  | Method -> `String "method"
  | Constructor -> `String "constructor"
  | ConstructorMethod -> `String "constructor-method"

let format_declaration_type = function
  | Var -> `String "var"
  | ArgumentArray -> `String "argarray"
  | ArgumentBinding i -> `Int i
  | CatchParam -> `String "catch"

let format_clean_event = function
  | CFunPre { f; base; args; call_type } ->
      `Assoc [("step", `String "funpre"); ("f", format_jsval f);
              ("base", format_jsval base); ("args", format_jsval args);
              ("call_type", format_call_type call_type)]
  | CFunPost { f; base; args; result; call_type } ->
      `Assoc [("step", `String "funpost"); ("f", format_jsval f);
              ("base", format_jsval base); ("args", format_jsval args);
              ("call_type", format_call_type call_type); ("result", format_jsval result)]
  | CLiteral { value; hasGetterSetter } ->
      `Assoc [("step", `String "literal"); ("val", format_jsval value);
              ("hasGetterSetter", `Bool hasGetterSetter)]
  | CForIn value -> `Assoc [("step", `String "forin"); ("val", format_jsval value)]
  | CDeclare { name; value; declaration_type } ->
      `Assoc [("step", `String "declare"); ("name", `String name);
              ("val", format_jsval value);
              ("decltype", format_declaration_type declaration_type)]
  | CGetField { base; offset; value } ->
      `Assoc [("step", `String "getfield"); ("base", format_jsval base);
              ("offset", `String offset); ("val", format_jsval value)]
  | CPutField { base; offset; value } ->
      `Assoc [("step", `String "putfield"); ("base", format_jsval base);
              ("offset", `String offset); ("val", format_jsval value)]
  | CGetFieldPre (base, offset) ->
      `Assoc [("step", `String "getfieldpre"); ("base", format_jsval base);
              ("offset", `String offset)]
  | CPutFieldPre { base; offset; value } ->
      `Assoc [("step", `String "putfieldpre"); ("base", format_jsval base);
              ("offset", `String offset); ("val", format_jsval value)]
  | CRead { name; value; isGlobal } ->
      `Assoc [("step", `String "read"); ("name", `String name);
              ("value", format_jsval value); ("isGlobal", `Bool isGlobal)]
  | CWrite { name; value; isGlobal; lhs; isSuccessful } ->
      `Assoc [("step", `String "read"); ("name", `String name);
              ("value", format_jsval value); ("isGlobal", `Bool isGlobal);
              ("lhs", format_jsval lhs); ("isSuccessful", `Bool isSuccessful)]
  | CReturn value -> `Assoc [("step", `String "return"); ("val", format_jsval value)]
  | CThrow value -> `Assoc [("step", `String "throw"); ("val", format_jsval value)]
  | CWith value -> `Assoc [("step", `String "with"); ("val", format_jsval value)]
  | CFunEnter { f; this; args } ->
      `Assoc [("step", `String "funenter"); ("f", format_jsval f);
              ("this", format_jsval this); ("args", format_jsval args)]
  | CFunExit { ret; exc } ->
      `Assoc [("step", `String "funexit"); ("ret", format_jsval ret);
              ("exc", format_jsval exc)]
  | CScriptEnter -> `Assoc [("step", `String "scriptenter")]
  | CScriptExit -> `Assoc [("step", `String "scriptexit")]
  | CScriptExc value -> `Assoc [("step", `String "scriptexc"); ("val", format_jsval value)]
  | CBinary { op; left; right; result } ->
      `Assoc [("step", `String "binary"); ("op", `String op);
              ("left", format_jsval left); ("right", format_jsval right);
              ("result", format_jsval result)]
  | CUnary { op; arg; result } ->
      `Assoc [("step", `String "unary"); ("op", `String op);
              ("arg", format_jsval arg); ("result", format_jsval result)]
  | CEndExpression -> `Assoc [("step", `String "endexpr")]
  | CConditional res ->
      `Assoc [("step", `String "conditional"); ("result", format_jsval res)]

let format_clean_trace tr = `List (List.map format_clean_event tr)
let format_clean_tracefile out (funs, objs, trace, globs, gap) =
  `Assoc [("func", format_functions funs);
          ("obj", format_objects objs);
          ("trace", format_clean_trace trace);
          ("globals", format_globals globs);
          ("globals_are_properties", `Bool gap)]
    |> to_channel out

let format_reference: Reference.reference -> json = let open Reference in function
  | LocalVariable var -> `Assoc [("local", `String var)]
  | GlobalVariable var -> `Assoc [("global", `String var)]
  | Field (obj, fld) -> `Assoc [("object", format_objectid obj); ("field", `String fld)]

let format_versioned_reference (ref, ver) = `List [format_reference ref; `Int ver]

let format_alias_source = function
  | Argument i -> `Assoc [("argument", `Int i)]
  | With r -> `Assoc [("with", format_versioned_reference r)]

let format_versions vers: json =
  `List (Reference.ReferenceMap.fold
             (fun ref ver json -> `List [format_reference ref; `Int ver] :: json)
             vers [])
let format_aliases aliases: json =
  `Assoc (StringMap.fold
            (fun name vref json -> (name, format_fieldref vref) :: json)
            aliases [])

let format_local_facts { last_arguments; last_update; versions; aliases } =
  let json = [ ("versions", format_versions versions);
               ("aliases", format_aliases aliases) ]
  in let json = match last_arguments with
    | Some arg -> ("arguments", `Int arg) :: json
    | None -> json
  in let json = match last_update with
    | Some upd -> ("update", format_versioned_reference upd) :: json
    | None -> json
  in `Assoc json

let format_enriched_trace fmtdata tr =
  `List (List.map (fun (op, data) -> `List [ format_clean_event op; fmtdata data ]) tr)

let format_facts_trace = format_enriched_trace format_local_facts
let format_unit_trace = format_enriched_trace (fun _ -> `Null)
let format_arguments_trace =
  format_enriched_trace (function
                           | Some arg -> `Assoc [("arguments", `Int arg)]
                           | None -> `Assoc [])

let format_rich_operation = function
  | RFunPre { f; base; args; call_type } ->
      `Assoc [("step", `String "funpre"); ("f", format_jsval f);
              ("base", format_jsval base); ("args", format_jsval args);
              ("call_type", format_call_type call_type)]
  | RFunPost { f; base; args; result; call_type } ->
      `Assoc [("step", `String "funpost"); ("f", format_jsval f);
              ("base", format_jsval base); ("args", format_jsval args);
              ("call_type", format_call_type call_type); ("result", format_jsval result)]
  | RLiteral { value; hasGetterSetter } ->
      `Assoc [("step", `String "literal"); ("val", format_jsval value);
              ("hasGetterSetter", `Bool hasGetterSetter)]
  | RForIn value -> `Assoc [("step", `String "forin"); ("val", format_jsval value)]
  | RLocal { name; ref } ->
      `Assoc [("step", `String "local"); ("name", `String name);
              ("ref", format_versioned_reference ref)]
  | RCatch { name; ref } ->
      `Assoc [("step", `String "catch"); ("name", `String name);
              ("ref", format_versioned_reference ref)]
  | RAlias { name; ref; source } ->
      `Assoc [("step", `String "catch"); ("name", `String name);
              ("ref", format_versioned_reference ref);
              ("source", format_alias_source source)]
  | RRead { ref; value } ->
      `Assoc [("step", `String "read");
              ("ref", format_versioned_reference ref);
              ("value", format_jsval value)]
  | RWrite { ref; oldref; value; success } ->
      `Assoc [("step", `String "read");
              ("ref", format_versioned_reference ref);
              ("oldref", format_versioned_reference oldref);
              ("value", format_jsval value);
              ("success", `Bool success)]
  | RReturn value -> `Assoc [("step", `String "return"); ("val", format_jsval value)]
  | RThrow value -> `Assoc [("step", `String "throw"); ("val", format_jsval value)]
  | RWith value -> `Assoc [("step", `String "with"); ("val", format_jsval value)]
  | RFunEnter { f; this; args } ->
      `Assoc [("step", `String "funenter"); ("f", format_jsval f);
              ("this", format_jsval this); ("args", format_jsval args)]
  | RFunExit { ret; exc } ->
      `Assoc [("step", `String "funexit"); ("ret", format_jsval ret);
              ("exc", format_jsval exc)]
  | RScriptEnter -> `Assoc [("step", `String "scriptenter")]
  | RScriptExit -> `Assoc [("step", `String "scriptexit")]
  | RScriptExc value -> `Assoc [("step", `String "scriptexc"); ("val", format_jsval value)]
  | RBinary { op; left; right; result } ->
      `Assoc [("step", `String "binary"); ("op", `String op);
              ("left", format_jsval left); ("right", format_jsval right);
              ("result", format_jsval result)]
  | RUnary { op; arg; result } ->
      `Assoc [("step", `String "unary"); ("op", `String op);
              ("arg", format_jsval arg); ("result", format_jsval result)]
  | REndExpression -> `Assoc [("step", `String "endexpr")]
  | RConditional res ->
      `Assoc [("step", `String "conditional"); ("result", format_jsval res)]

let format_rich_event (op, lf) =
  `List [format_rich_operation op; format_local_facts lf]
let format_rich_event_small (op, { versions }) =
  `List [format_rich_operation op; format_versions versions]

let format_rich_trace tr = `List (List.map format_rich_event tr)
let format_rich_trace_small tr = `List (List.map format_rich_event_small tr)

let format_points_to ptm =
  `List (Reference.VersionReferenceMap.fold
           (fun vref value json ->
              `List [format_versioned_reference vref; format_jsval value] :: json)
           ptm [])

let format_rich_tracefile
      c { funcs; objs; trace; globals; globals_are_properties; points_to } =
  `Assoc [
    ("funcs", format_functions funcs);
    ("objs", format_objects objs);
    ("trace", format_rich_trace trace);
    ("globals", format_globals globals);
    ("globals_are_properties", `Bool globals_are_properties);
    ("pointsto", format_points_to points_to)
  ] |> to_channel c

let format_rich_tracefile_small
      c { funcs; objs; trace; globals; globals_are_properties; points_to } =
  `Assoc [
    ("funcs", format_functions funcs);
    ("objs", format_objects objs);
    ("trace", format_rich_trace_small trace);
    ("globals", format_globals globals);
    ("globals_are_properties", `Bool globals_are_properties);
    ("pointsto", format_points_to points_to)
  ] |> to_channel c

let string_of_points_to pt = format_points_to pt |> Yojson.Basic.to_string
let string_of_rich_event_small re = format_rich_event_small re |> Yojson.Basic.to_string
let string_of_rich_event re = format_rich_event re |> Yojson.Basic.to_string
let string_of_rich_operation re = format_rich_operation re |> Yojson.Basic.to_string
let string_of_clean_event ev = format_clean_event ev |> Yojson.Basic.to_string


