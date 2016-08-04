open TypesJS
open TraceTypes

open Yojson.Basic;;
open Yojson.Basic.Util;;

exception ParseError
let report err msg json = 
  Log.err (fun m -> m "%s - %s: %s" err msg (Yojson.Basic.to_string json));
  raise ParseError

let to_string err json =
  try to_string json with Type_error (msg, json) -> report err msg json

let parse_jsval json =
  try
    match member "type" json |> to_string "Value type" with
      | "undefined" -> OUndefined
      | "boolean" -> OBoolean (member "val" json |> to_string "Value" |> bool_of_string)
      | "number" ->
          let numstr = member "val" json |> to_string "Value" in begin
            try ONumberInt (BatInt.of_string numstr)
            with Failure _ -> begin
              try ONumberFloat (BatFloat.of_string numstr)
              with Failure _ ->
                failwith ("Strange number here: " ^ Yojson.Basic.to_string json)
                | e -> Format.eprintf "Unexpected exception %s for BatFloat.of_string@." (Printexc.to_string e); raise e
            end
              | e -> Format.eprintf "Unexpected exception %s for BatInt.of_string@." (Printexc.to_string e); raise e
          end
      | "string" -> OString (member "val" json |> to_string "Value")
      | "symbol" -> OSymbol (member "val" json |> to_string "Value")
      | "function" -> OFunction (member "id" json |> to_int, member "funid" json |> to_int)
      | "null" -> ONull
      | "object" -> OObject (member "id" json |> to_int)
      | _ as ty -> OOther (ty, member "id" json |> to_int)
  with
    | ParseError -> report "Context" "jsval" json
    | Type_error (msg, json) -> report "parse_jsval" msg json

let native_pattern = "[native code]"
let parse_funcspec json =
  try
    let instr = member "instrumented" json |> to_string "Function specification" in
      if (BatString.Exceptionless.find instr native_pattern <> None)
      then External (json |> member "obj" (*|> member "funid"*) |> to_int)
      else match json |> member "uninstrumented" |> to_string_option with
        | Some uninstr -> OrigCode (instr, uninstr)
        | None -> ReflectedCode instr
  with
    | ParseError -> report "Context" "funcspec" json
    | Type_error (msg, json) -> report "parse_funcspec" msg json

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

let parse_intmap parse_value json =
  List.fold_left (fun map (key, json) ->
                    try
                      let key' = int_of_string key in
                        CCIntMap.add key' (parse_value json) map
                    with
                      | Failure _ -> map
                      | e -> Format.eprintf "Exception %s for %s@." (Printexc.to_string e) (Yojson.Basic.to_string json); raise e)
    CCIntMap.empty (to_assoc json)

let parse_single_iidmap json =
  let parse_location json =
    match to_list json with
      | [ `Int first_line; `Int first_char; `Int last_line; `Int last_char ] ->
          { first_line; first_char; last_line; last_char }
      | _ ->
          report "IID" "locations" json
      | exception e ->
          Format.eprintf "Exception %s for %s@." (Printexc.to_string e) (Yojson.Basic.to_string json); raise e
  in 
    parse_intmap parse_location json

let parse_iidmap json =
  try
    parse_intmap parse_single_iidmap json
  with ParseError -> report "Context" "iidmap" json
    | e -> Format.eprintf "Exception %s for %s@." (Printexc.to_string e) (Yojson.Basic.to_string json); raise e

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
    | "conditional" -> Conditional (get_int "iid", get_jsval "result")
    | "switchscript" -> SwitchScript (get_int "sid")
    | _ as op -> failwith ("Unknown event " ^ op)
  with ParseError -> report "Context" "event" json

let parse_functions json =
  try json |> convert_each parse_funcspec |> BatDynArray.of_list
  with ParseError -> report "Context" "functions" json

let parse_objects json =
  try json |> convert_each parse_objectspec |> BatDynArray.of_list
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

let parse_tracefile source: tracefile =
  let json = from_channel source in
  (parse_functions (member "func" json),
   parse_objects (member "obj" json),
   parse_trace (member "trace" json),
   parse_globals (member "globals" json),
   to_bool (member "globals_are_properties" json),
   (*try*) parse_iidmap (member "iid" json) (*with _ -> CCIntMap.empty*)
  )

let event_of_string str = from_string str |> parse_operation
let jsval_of_string str = from_string str |> parse_jsval
let objectspec_of_string str = from_string str |> parse_objectspec
let funcspec_of_string str = from_string str |> parse_funcspec

let serialize_tracefile
      (functions, objects, trace, globals, globals_are_properties, iids) =
  Marshal.to_bytes
    (BatDynArray.to_list functions,
     BatDynArray.to_list objects,
     trace,
     globals,
     globals_are_properties,
     iids) []

let deserialize_tracefile bytes =
  let (functions, objects, trace, globals, globals_are_properties, iids) =
    (Marshal.from_bytes bytes 0: funcspec list * objectspec list * trace * globals * bool * iidmap)
  in (BatDynArray.of_list functions,
      BatDynArray.of_list objects,
      trace,
      globals,
      globals_are_properties,
      iids)

let with_file filename handler =
  let chan = open_in filename in
    try
      let result = handler chan in close_in chan; result
    with e -> close_in chan; raise e

let readbufsize = 1024
let bytes_from_file handler file =
  let buffer = Buffer.create 1024 in
  let readbuf = Bytes.create readbufsize in
  let rec fill () =
    if input file readbuf 0 readbufsize = 0 then
      Buffer.to_bytes buffer
    else begin
      Buffer.add_bytes buffer readbuf;
      fill ()
    end
  in handler (fill ())

let read_tracefile filename =
  if Filename.check_suffix filename ".bin" then
    with_file filename (bytes_from_file deserialize_tracefile)
  else
    with_file filename parse_tracefile 
