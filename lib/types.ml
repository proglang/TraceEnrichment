let shorten ?(lenbound = 20) s =
  let len = String.length s in
  try
    let idx = String.index s '\n' in
      String.sub s 0 (min idx lenbound) ^ "..."
  with Not_found ->
    if len < lenbound then s else String.sub s 0 lenbound ^ "..."

let pp_shortened pp s = Fmt.string pp (shorten s)
let (%%) = Fmt.const
let (%<) = Fmt.prefix
let (%>) = Fmt.suffix
let (!%) = Fmt.const Fmt.string
type jsval =
  | OUndefined [@printer Fmt.string %% "undefined"]
  | ONull [@printer Fmt.string %% "null"]
  | OBoolean of bool [@printer Fmt.bool]
  | ONumberInt of int [@printer Fmt.int]
  | ONumberFloat of float [@printer Fmt.float]
  | OString of string [@printer (!% "\"") %< pp_shortened %> (!% "\"")]
  | OSymbol of string [@printer (!% "symbol:") %< Fmt.braces pp_shortened]
  | OFunction of int * int [@printer (!% "function:") %< (Fmt.pair ~sep:(!% "/") Fmt.int Fmt.int)]
  | OObject of int [@printer (!% "object:") %< Fmt.int]
  | OOther of string * int [@printer (Fmt.pair ~sep:(!% ":") Fmt.string Fmt.int)]
  (*[@@deriving show, eq, ord]*)
  [@@deriving ord]
let equal_jsval x y = compare_jsval x y = 0

let pp_jsval pp = let open Format in function
    | OUndefined -> pp_print_string pp "undefined"
    | ONull -> pp_print_string pp "null"
    | OBoolean x -> fprintf pp "bool:%b" x
    | ONumberInt x -> fprintf pp "int:%d" x
    | ONumberFloat x -> fprintf pp "float:%f" x
    | OString x -> fprintf pp "string:%s" (shorten x)
    | OSymbol x -> fprintf pp "symbol:%s" (shorten x)
    | OFunction (id, fid) -> fprintf pp "function:%d/%d" id fid
    | OObject id -> fprintf pp "object:%d" id
    | OOther (ty, id) -> fprintf pp "other:%s:%d" ty id

type fieldspec = {
  value: jsval;
  writable: bool [@default true];
  get: jsval option;
  set: jsval option;
  enumerable: bool [@default true];
  configurable: bool [@default true]
} [@@deriving eq, ord, make]

let pp_fieldspec pp { value; set; get; writable; enumerable; configurable } =
  (* Special-case the most common case *)
  if writable && enumerable && configurable && set = None && get = None then
    pp_jsval pp value
  else if set = None && get = None then
    Format.fprintf pp "[%s%s%s] %a"
      (if writable then "W" else "-")
      (if enumerable then "E" else "-")
      (if configurable then "C" else "-")
      pp_jsval value
  else
    Format.fprintf pp "[%s%s%s] %a { get = %a, set = %a }"
      (if writable then "W" else "-")
      (if enumerable then "E" else "-")
      (if configurable then "C" else "-")
      pp_jsval value
      (Fmt.option pp_jsval) get
      (Fmt.option pp_jsval) set

let pp_dynarray fmt =
  Fmt.using (BatDynArray.mapi (fun i v -> (i,v)))
    (Fmt.using BatDynArray.to_list
       (Fmt.vbox (Fmt.list ~sep:(Fmt.always ",@ ")
                    (Fmt.pair ~sep:(Fmt.always ". ") Fmt.int fmt))))

type objectspec = fieldspec StringMap.t [@@deriving show, eq]
type objects = objectspec BatDynArray.t
let pp_objects = pp_dynarray (Fmt.vbox pp_objectspec)
let equal_objects o1 o2 =
  BatEnum.equal equal_objectspec (BatDynArray.enum o1) (BatDynArray.enum o2)

type funcspec =
  | ReflectedCode of string [@printer (!% "instrumented: ") %< pp_shortened]
  | OrigCode of string * string
  | External of int [@printer (!% "external: ") %< Fmt.int]
  [@@deriving ord, eq, show]

type functions = funcspec BatDynArray.t
let pp_functions = pp_dynarray pp_funcspec
let equal_functions f1 f2 =
  BatEnum.equal equal_funcspec (BatDynArray.enum f1) (BatDynArray.enum f2)

type globals = jsval StringMap.t [@@deriving show, eq]

exception NotAnObject
let get_object = function
  | OObject id -> id
  | OOther (_, id) -> id
  | OFunction (id, _) -> id
  | _ -> raise NotAnObject

type objectid =
  | Object of int
  | Function of int * int
  | Other of string * int
  [@@deriving show, ord, eq]

type fieldref = objectid * string [@@deriving show, ord, eq];;

let objectid_to_jsval = function
  | Object o -> OObject o
  | Function (o, f) -> OFunction (o, f)
  | Other (t, o) -> OOther (t, o)

let objectid_of_jsval = function
  | OObject o -> Object o
  | OFunction (o, f) -> Function (o, f)
  | OOther (t, o) -> Other (t, o)
  | _ -> raise NotAnObject

let get_object_id = function
  | Object id | Function (id, _) | Other (_, id) -> id

let try_objectid_of_jsval = function
  | OObject o -> Some (Object o)
  | OFunction (o, f) -> Some (Function (o, f))
  | OOther (t, o) -> Some (Other (t, o))
  | _ -> None

let try_get_object = function
  | OObject id -> Some id
  | OOther (_, id) -> Some id
  | OFunction (id, _) -> Some id
  | _ -> None

let is_base = function
  | OObject _
  | OOther _
  | OFunction _ -> false
  | _ -> true

(** {1 IID maps} *)
type location = {
  first_line: int;
  last_line: int;
  first_char: int;
  last_char: int
}
let pp_location pp { first_line; last_line; first_char; last_char } =
  Fmt.pf pp "[%d:%d - %d:%d]" first_line first_char last_line last_char

type iidmap = location CCIntMap.t
let pp_iidmap =
  Fmt.iter_bindings CCIntMap.iter
    (Fmt.pair Fmt.int pp_location)
let equal_iidmap: iidmap -> iidmap -> bool = CCIntMap.equal ~eq:(=)

type initials = {
  functions: functions;
  objects: objects;
  globals: globals;
  globals_are_properties: bool;
  iids: iidmap;
  mutable function_apply: jsval;
  mutable function_call: jsval;
  mutable function_constructor: jsval;
  mutable function_eval: jsval;
} [@@deriving show, eq]

exception ObjectNotFound
let lookup_object ?(required=false) (objects: objects) objval fieldname =
  try
    BatDynArray.get objects (get_object objval)
    |> StringMap.find fieldname
    |> fun { value } -> value
  with Not_found ->
    Log.debug (fun m -> m "Could not find object %a, field %s"
      pp_jsval objval fieldname);
    if required then
      raise ObjectNotFound
    else
      OUndefined

let lookup { globals; objects = objs } path =
  try
    List.fold_left (lookup_object ~required:true objs)
      (StringMap.find "global" globals)
      path
  with
      Not_found ->
        Format.eprintf "Can't find %a@."
          (Fmt.list ~sep:(Fmt.const Fmt.string ".") Fmt.string)
          path;
        raise Not_found

let lookup_functions initials =
  initials.function_apply <- lookup initials ["Function"; "prototype"; "apply"];
  initials.function_call  <- lookup initials ["Function"; "prototype"; "call"];
  initials.function_constructor <- lookup initials ["Function"];
  initials.function_eval <- lookup initials ["eval"]


