open Types
(** The different kinds of traces. *)
(** {1 Structures that contain event data} *)
(** Thanks to these structures, we do not carry around huge tuples. The fields are exactly
 the properties that can be read off from the JSON trace representation, unless otherwise noted. *)
type raw_funpre = {
  f : jsval;
  base : jsval;
  args : jsval;
  isConstructor : bool;
  isMethod : bool;
}
type raw_funpost = {
  f : jsval;
  base : jsval;
  args : jsval;
  result : jsval;
  isConstructor : bool;
  isMethod : bool;
}
type literal = { value : jsval; hasGetterSetter : bool; }
type raw_declare = {
  name : string;
  value : jsval;
  argument : int option;
  isCatchParam : bool;
}
type raw_getfieldpre = {
  base : jsval;
  offset : string;
  isComputed : bool;
  isOpAssign : bool;
  isMethodCall : bool;
}
type raw_getfieldpost = {
  base : jsval;
  offset : string;
  value : jsval;
  isComputed : bool;
  isOpAssign : bool;
  isMethodCall : bool;
}
type raw_putfield = {
  base : jsval;
  offset : string;
  value : jsval;
  isComputed : bool;
  isOpAssign : bool;
}
type raw_access = {
  name : string;
  value : jsval;
  isGlobal : bool;
  isScriptLocal : bool;
}
type raw_writeaccess = {
  name : string;
  lhs : jsval;
  value : jsval;
  isGlobal : bool;
  isScriptLocal : bool;
}
type raw_binpre = {
  op : string;
  left : jsval;
  right : jsval;
  isOpAssign : bool;
  isSwitchCaseComparison : bool;
  isComputed : bool;
}
type raw_binary = {
  op : string;
  left : jsval;
  right : jsval;
  result : jsval;
  isOpAssign : bool;
  isSwitchCaseComparison : bool;
  isComputed : bool;
}
type raw_unpre = { op : string; arg : jsval; }
type unary = { op : string; arg : jsval; result : jsval; }
type funenter = { f : jsval; this : jsval; args : jsval; }
type funexit = { ret : jsval; exc : jsval; }
(** The type of operations in a trace.
  This covers exactly the possible cases in the JSON trace. *)
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
(** A trace is a sequence of events. *)
type trace = event list
type raw_stream = event Streaming.Stream.t

(** A trace file is a tuple containing the various components defined above. *)
type tracefile = functions * objects * trace * globals * bool * iidmap

(** {1 Cleaned-up traces} *)
(** A cleaned-up trace is a version of a trace that has
 unneccesary detail removed, follows the trace grammar and
 contains some extra information. *)

(** Classification of the different types of function calls. This just enumerates
 the combinations of flags. *)
type call_type = Function | Method | Constructor

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

(** Structures that contain information about the different possible events on a trace.

 These structures are pruned-down versions of those in [Trace].
*)
type funpre = {
  f : jsval;
  base : jsval;
  args : jsval;
  call_type: call_type
}
type funpost = {
  f : jsval;
  base : jsval;
  args : jsval;
  result : jsval;
  call_type: call_type
}
type declare = {
  name : string;
  value : jsval;
  declaration_type: declaration_type
}
type accessfield = {
  base : jsval;
  offset : string;
  value : jsval
}
type read = {
  name : string;
  value : jsval
}
type write = {
  name : string;
  lhs : jsval;
  value : jsval;
  isSuccessful: bool
}
type binary = {
  op : string;
  left : jsval;
  right : jsval;
  result : jsval
}

(** Events that can occur in a cleaned-up trace. Note that certain
 events in a [Trace.trace] are redundant for out task, so we drop them. *)
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

(** A clean trace is a list of cleaned-up events. *)
type clean_trace = clean_operation list
type clean_stream = clean_operation Streaming.Stream.t
(** A clean trace file is like a trace file, only it contains a clean trace. *)
type clean_tracefile = functions * objects * clean_trace * globals * bool * iidmap

(** {1 A nicer form of trace, with more uniform events} *)

(** This contains an explanation of where an alias comes from. *)
type alias_source = Argument of int | With of Reference.versioned_reference

(** Structures that sum up data about certain operations. *)
type local = {
  name : string;
  ref : Reference.versioned_reference;
}
type alias = {
  name : string;
  source : alias_source;
  ref : Reference.versioned_reference;
}
type rread = {
  ref : Reference.versioned_reference;
  value : jsval;
}
type rwrite = {
  ref : Reference.versioned_reference;
  oldref: Reference.versioned_reference;
  value : jsval;
  success : bool;
}
(** Events that make use of the facts calculated by the [LocalFacts] module
 and consorts to provide a better representation for trace comparison.
 Compare with [clean_operation], and note that variable and field accessed
 have been unified to [RRead] and [RWrite], while [CDeclare] has been split
 into [RAlias], [RLocal] and [RCatch]. *)
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

(** Traces and tracefiles enrichted with local facts. *)
type 'a enriched_trace = (clean_operation * 'a) list
type 'a enriched_tracefile = functions * objects * 'a enriched_trace * globals * bool * iidmap
type 'a enriched_stream = (clean_operation * 'a) Streaming.Stream.t

val pp_enriched_trace: (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a enriched_trace -> unit
val pp_enriched_tracefile: (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a enriched_tracefile -> unit

(** Rich facts [rich_facts] contains a subset of local facts. *)
type rich_facts = {
  last_update: Reference.versioned_reference option;
  versions: int Reference.ReferenceMap.t;
  points_to: Reference.points_to_map;
  names: Reference.reference StringMap.t
}

(** A rich event is a pair of a rich operation and the associated local facts *)
type rich_event = rich_operation * rich_facts
(** A rich trace contains rich operations and local facts. *)
type rich_trace = rich_event list
(** A rich trace file contains the original function and object descriptions,
 as well as global object information and the [globals_are_properties] flag.
 Furthermore, it contains a rich trace and a points - to map for the references
 occuring in the program. *)
type rich_tracefile = {
  funcs : functions;
  objs : objects;
  trace : rich_trace;
  globals : globals;
  globals_are_properties : bool;
  points_to : Reference.points_to_map;
  iidmap: iidmap
}
type rich_stream = rich_event Streaming.Stream.t

(** Pretty printers. *)
val pp_operation : Format.formatter -> event -> unit
val pp_trace : Format.formatter -> event list -> unit
val pp_tracefile : Format.formatter -> tracefile -> unit
val pp_call_type : Format.formatter -> call_type -> unit
val pp_clean_operation : Format.formatter -> clean_operation -> unit
val pp_clean_trace : Format.formatter -> clean_trace -> unit
val pp_clean_tracefile : Format.formatter -> clean_tracefile -> unit
val pp_alias_source : Format.formatter -> alias_source -> unit
val pp_rich_operation : Format.formatter -> rich_operation -> unit
val pp_rich_trace : Format.formatter -> rich_trace -> unit
val pp_rich_tracefile : Format.formatter -> rich_tracefile -> unit

(** Debugging helper: When writing rich events, also dump associated facts. *)
val enable_dump_facts: unit -> unit
