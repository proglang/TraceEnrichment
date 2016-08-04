open TypesJS

type raw_funpre = {
  f : jsval;
  base : jsval;
  args : jsval;
  isConstructor : bool;
  isMethod : bool;
} [@@deriving show]
type raw_funpost = {
  f : jsval;
  base : jsval;
  args : jsval;
  result : jsval;
  isConstructor : bool;
  isMethod : bool;
} [@@deriving show]
type literal = { value : jsval; hasGetterSetter : bool; } [@@deriving show]
type raw_declare = {
  name : string;
  value : jsval;
  argument : int option;
  isCatchParam : bool;
} [@@deriving show]
type raw_getfieldpre = {
  base : jsval;
  offset : string;
  isComputed : bool;
  isOpAssign : bool;
  isMethodCall : bool;
} [@@deriving show]
type raw_getfieldpost = {
  base : jsval;
  offset : string;
  value : jsval;
  isComputed : bool;
  isOpAssign : bool;
  isMethodCall : bool;
} [@@deriving show]
type raw_putfield = {
  base : jsval;
  offset : string;
  value : jsval;
  isComputed : bool;
  isOpAssign : bool;
} [@@deriving show]
type raw_access = {
  name : string;
  value : jsval;
  isGlobal : bool;
  isScriptLocal : bool;
} [@@deriving show]
type raw_writeaccess = {
  name : string;
  lhs : jsval;
  value : jsval;
  isGlobal : bool;
  isScriptLocal : bool;
} [@@deriving show]
type raw_binpre = {
  op : string;
  left : jsval;
  right : jsval;
  isOpAssign : bool;
  isSwitchCaseComparison : bool;
  isComputed : bool;
} [@@deriving show]
type raw_binary = {
  op : string;
  left : jsval;
  right : jsval;
  result : jsval;
  isOpAssign : bool;
  isSwitchCaseComparison : bool;
  isComputed : bool;
} [@@deriving show]
type raw_unpre = { op : string; arg : jsval; } [@@deriving show]
type unary = { op : string; arg : jsval; result : jsval; } [@@deriving show]
type funenter = { f : jsval; this : jsval; args : jsval; } [@@deriving show]
type funexit = { ret : jsval; exc : jsval; } [@@deriving show]
(** The type of operations in a trace.
  * This covers exactly the possible cases in the JSON trace. *)
type event =
  | FunPre of int * raw_funpre
  | FunPost of int * raw_funpost
  | Literal of int * literal
  | ForIn of int * jsval
  | Declare of int * raw_declare
  | GetFieldPre of int * raw_getfieldpre
  | GetField of int * raw_getfieldpost
  | PutFieldPre of int * raw_putfield
  | PutField of int * raw_putfield
  | Read of int * raw_access
  | Write of int * raw_writeaccess
  | Return of int * jsval
  | Throw of int * jsval
  | With of int * jsval
  | FunEnter of int * funenter
  | FunExit of int * funexit
  | ScriptEnter
  | ScriptExit
  | ScriptExc of jsval
  | BinPre of int * raw_binpre
  | BinPost of int * raw_binary
  | UnaryPre of int * raw_unpre
  | UnaryPost of int * unary
  | EndExpression of int
  | Conditional of int * jsval
  | SwitchScript of int
  [@@deriving show]
  (** A trace is a sequence of events. *)
type trace = event list [@@deriving show]
type raw_stream = event Streaming.Stream.t

(** A trace file is a tuple containing the various components defined above. *)
type tracefile = functions * objects * trace * globals * bool * iidmap [@@deriving show]

(** * Cleaned-up traces. *)
(** A cleaned-up trace is a version of a trace that has
  * unneccesary detail removed, and contains proper information
  * on whether a variable access goes to a global or a local variable. *)

(** Classification of the different types of function calls. This just enumerates
  * the combinations of flags. *)
type call_type = Function | Method | Constructor
  [@@deriving show]

(** The different possible types of variable declarations. *)
type declaration_type =
  (** A regular local variable. *)
  | Var
  (** The argument array [arguments] that is implicitly bound when entering a function. *)
  | ArgumentArray
  (** The variable binds to the [i]-th argument. Note that this is an alias. *)
  | ArgumentBinding of int
  (** The variable binds a caught exception. *)
  | CatchParam
  [@@deriving show]

(** Structures that contain information about the different possible events on a trace.
  * 
  * These structures are pruned-down versions of those in [Trace].
*)
type funpre = {
  f : jsval;
  base : jsval;
  args : jsval;
  call_type: call_type
} [@@deriving show]
type funpost = {
  f : jsval;
  base : jsval;
  args : jsval;
  result : jsval;
  call_type: call_type
} [@@deriving show]
type declare = {
  name : string;
  value : jsval;
  declaration_type: declaration_type
} [@@deriving show]
type accessfield = {
  base : jsval;
  offset : string;
  value : jsval;
  isComputed : bool;
} [@@deriving show]
type read = {
  name : string;
  value : jsval
} [@@deriving show]
type write = {
  name : string;
  lhs : jsval;
  value : jsval;
  isSuccessful: bool
} [@@deriving show]
type binary = {
  op : string;
  left : jsval;
  right : jsval;
  result : jsval
} [@@deriving show]

(** Events that can occur in a cleaned-up trace. Note that certain
  * events in a [Trace.trace] are redundant for out task, so we drop them. *)
type clean_operation =
  | CFunPre of funpre
  | CFunPost of funpost
  | CLiteral of literal
  | CForIn of jsval
  | CDeclare of declare
  | CGetFieldPre of (jsval * string)
  | CPutFieldPre of accessfield
  | CGetField of accessfield
  | CPutField of accessfield
  | CRead of read
  | CWrite of write
  | CReturn of jsval
  | CThrow of jsval
  | CWith of jsval
  | CFunEnter of funenter
  | CFunExit of funexit
  | CScriptEnter
  | CScriptExit
  | CScriptExc of jsval
  | CBinary of binary
  | CUnary of unary
  | CEndExpression
  | CConditional of location option * jsval
  [@@deriving show]
(** A clean trace is a list of cleaned-up events. *)
type clean_trace = clean_operation list [@@deriving show]
(** A clean trace file is like a trace file, only it contains a clean trace. *)
type clean_tracefile = functions * objects * clean_trace * globals * bool * iidmap [@@deriving show]
type clean_stream = clean_operation Streaming.Stream.t

open Format

(** This contains an explanation of where an alias comes from. *)
type alias_source = Argument of int | With of Reference.versioned_reference  [@@deriving show]

(** Structures that sum up data about certain operations. *)
type local = {
  name : string;
  ref : Reference.versioned_reference;
} [@@deriving show]
type alias = {
  name : string;
  source : alias_source;
  ref : Reference.versioned_reference;
} [@@deriving show]
type rread = {
  ref : Reference.versioned_reference;
  value : jsval;
  isComputed : bool;
} [@@deriving show]
type rwrite = {
  ref : Reference.versioned_reference;
  oldref: Reference.versioned_reference;
  value : jsval;
  success : bool;
  isComputed : bool;
} [@@deriving show]

type 'a enriched_trace = (clean_operation * 'a) list [@@deriving show]
type 'a enriched_tracefile = functions * objects * 'a enriched_trace * globals * bool * iidmap [@@deriving show]
type 'a enriched_stream = (clean_operation * 'a) Streaming.Stream.t

(** Events that make use of the facts calculated by the [LocalFacts] module
  * and consorts to provide a better representation for trace comparison.
  * Compare with [clean_operation], and note that variable and field accessed
  * have been unified to [RRead] and [RWrite], while [CDeclare] has been split
  * into [RAlias], [RLocal] and [RCatch]. *)
type rich_operation =
    RFunPre of funpre
  | RFunPost of funpost
  | RLiteral of literal
  | RForIn of jsval
  | RLocal of local
  | RCatch of local
  | RAlias of alias
  | RRead of rread
  | RWrite of rwrite
  | RReturn of jsval
  | RThrow of jsval
  | RWith of jsval
  | RFunEnter of funenter
  | RFunExit of funexit
  | RScriptEnter
  | RScriptExit
  | RScriptExc of jsval
  | RBinary of binary
  | RUnary of unary
  | REndExpression
  | RConditional of location option * jsval
  [@@deriving show]
(** Rich facts [rich_facts] contains a subset of local facts. *)
type rich_facts = {
  last_update: Reference.versioned_reference option;
  versions: int Reference.ReferenceMap.t;
  points_to: Reference.points_to_map;
  names: Reference.reference StringMap.t
} [@@deriving show]

(** A rich event is a pair of a rich operation and the associated local facts *)
type rich_event = rich_operation * rich_facts [@@deriving show]
(** A rich trace contains rich operations and local facts. *)
type rich_trace = rich_event list [@@deriving show]
(** A rich trace file contains the original function and object descriptions,
  * as well as global object information and the [globals_are_properties] flag.
  * Furthermore, it contains a rich trace and a points - to map for the references
  * occuring in the program. *)
type rich_tracefile = {
  funcs : functions;
  objs : objects;
  trace : rich_trace;
  globals : globals;
  globals_are_properties : bool;
  points_to : Reference.points_to_map;
  iidmap: iidmap
} [@@deriving show]
type rich_stream = rich_event Streaming.Stream.t

let dump_facts = ref false
let enable_dump_facts () = dump_facts := true
let pp_operation = pp_event

