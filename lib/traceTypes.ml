open Types

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
  | Conditional of jsval
  (** A trace is a sequence of events. *)
type trace = event list
type raw_stream = event Streaming.Stream.t

(** A trace file is a tuple containing the various components defined above. *)
type tracefile = functions * objects * trace * globals * bool

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
  | CConditional of jsval

(** A clean trace is a list of cleaned-up events. *)
type clean_trace = clean_operation list
(** A clean trace file is like a trace file, only it contains a clean trace. *)
type clean_tracefile = functions * objects * clean_trace * globals * bool
type clean_stream = clean_operation Streaming.Stream.t

open Format

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

type local_facts = {
  last_arguments: int option;
  last_update: Reference.versioned_reference option;
  versions: int Reference.ReferenceMap.t;
  aliases: fieldref StringMap.t;
  points_to: Reference.points_to_map
}

type 'a enriched_trace = (clean_operation * 'a) list
type 'a enriched_tracefile = functions * objects * 'a enriched_trace * globals * bool
type facts_trace = local_facts enriched_trace
type facts_tracefile = local_facts enriched_tracefile
type full_facts_trace = local_facts enriched_trace
type full_facts_tracefile = local_facts enriched_tracefile
type unit_trace = unit enriched_trace
type unit_tracefile = unit enriched_tracefile
type arguments_trace = int option enriched_trace
type arguments_tracefile = int option enriched_tracefile
type 'a enriched_stream = (clean_operation * 'a) Streaming.Stream.t
type facts_stream = local_facts enriched_stream
type full_facts_stream = local_facts enriched_stream
type arguments_stream = int option enriched_stream

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
  | RConditional of jsval

(** A rich event is a pair of a rich operation and the associated local facts *)
type rich_event = rich_operation * local_facts
(** A rich trace contains rich operations and local facts. *)
type rich_trace = rich_event list
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
}
type rich_stream = rich_event Streaming.Stream.t

let pp_operation pp = function
  | FunPre (_, { isConstructor; isMethod; f; base; args }) ->
    fprintf pp "FunPre(f=%a, base=%a, args=%a, isConstructor=%b, isMethod = %b"
      pp_jsval f pp_jsval base pp_jsval args isConstructor isMethod
  | FunPost (_, { isConstructor; isMethod; f; base; args; result }) ->
    fprintf pp "FunPost(f=%a, base=%a, args=%a, result=%a, isConstructor=%b, isMethod = %b"
      pp_jsval f pp_jsval base pp_jsval args pp_jsval result isConstructor isMethod
  | Literal (_, { value; hasGetterSetter }) -> fprintf pp "Literal(value=%a, hasGetterSetter=%b)" pp_jsval value hasGetterSetter
  | ForIn (_, value) -> fprintf pp "ForIn(value=%a)" pp_jsval value
  | Declare (_, { name; value; argument; isCatchParam }) ->
    fprintf pp "Declare(name=%s, value=%a, argument=%a, isCatchParam=%b)"
      name pp_jsval value (Fmt.option Fmt.int) argument isCatchParam
  | GetFieldPre (_, { base; offset; isOpAssign; isMethodCall }) ->
    fprintf pp "GetFieldPre(base=%a, offset=%s, isOpAssign=%b, isMethodCall=%b)"
      pp_jsval base offset isOpAssign isMethodCall
  | GetField (_, { base; offset; value; isOpAssign; isMethodCall }) ->
    fprintf pp "GetFieldPre(base=%a, offset=%s, result=%a, isOpAssign=%b, isMethodCall=%b)"
      pp_jsval base offset pp_jsval value isOpAssign isMethodCall
  | PutFieldPre (_, { value; base; offset; isOpAssign }) ->
    fprintf pp "PutFieldPre(base=%a, offset=%s, value=%a, isOpAssign=%b"
      pp_jsval base offset pp_jsval value isOpAssign
  | PutField (_, { value; base; offset; isOpAssign }) ->
    fprintf pp "PutField(base=%a, offset=%s, value=%a, isOpAssign=%b"
      pp_jsval base offset pp_jsval value isOpAssign
  | Read (_, { name; isGlobal; isScriptLocal; value }) ->
    fprintf pp "Read(name=%s, value=%a, isGlobal=%b, isScriptLocal=%b"
      name pp_jsval value isGlobal isScriptLocal
  | Write (_, { name; isGlobal; isScriptLocal; value }) ->
    fprintf pp "Write(name=%s, value=%a, isGlobal=%b, isScriptLocal=%b"
      name pp_jsval value isGlobal isScriptLocal
  | Return (_, value) -> fprintf pp "Return(%a)" pp_jsval value
  | Throw (_, value) -> fprintf pp "Throw(%a)" pp_jsval value
  | With (_, value) -> fprintf pp "With(%a)" pp_jsval value
  | FunEnter (_, { f; this; args }) -> fprintf pp "FunEnter(f=%a, this=%a, args=%a)" pp_jsval f pp_jsval this pp_jsval args
  | FunExit (_, { ret; exc }) -> fprintf pp "FunExit(ret=%a, exc=%a)" pp_jsval ret pp_jsval exc
  | ScriptEnter -> pp_print_string pp "ScriptEnter"
  | ScriptExit -> pp_print_string pp "ScriptExit"
  | ScriptExc exc -> fprintf pp "script exit with exception %a" pp_jsval exc
  | BinPre (_, { left; right; op; isOpAssign; isSwitchCaseComparison }) ->
    fprintf pp "BinPre(left=%a, op=%s, right=%a, isOpAssign=%b, isSwitchCaseComparison=%b"
      pp_jsval left op pp_jsval right isOpAssign isSwitchCaseComparison
  | BinPost (_, { left; right; op; result; isOpAssign; isSwitchCaseComparison }) ->
    fprintf pp "BinPost(left=%a, op=%s, right=%a, result=%a, isOpAssign=%b, isSwitchCaseComparison=%b"
      pp_jsval left op pp_jsval right pp_jsval result isOpAssign isSwitchCaseComparison
  | UnaryPre (_, { op; arg }) ->
    fprintf pp "UnaryPre(op=%s, arg=%a)" op pp_jsval arg
  | UnaryPost (_, { op; arg; result }) ->
    fprintf pp "UnaryPost(op=%s, arg=%a, result=%a)" op pp_jsval arg pp_jsval result
  | EndExpression iid -> pp_print_string pp "EndExpression"
  | Conditional value -> fprintf pp "Conditional(value=%a)" pp_jsval value

let pp_trace pp trace =
  pp_open_vbox pp 0;
  List.iter (fprintf pp "@[<h>%a@];@ " pp_operation) trace;
  pp_close_box pp ()

let pp_tracefile pp (f, o, t, g, gap) =
  fprintf pp "@[<v>Globals are properties: %b@ @[<hov>%a@]@ @[<hov>%a@]@ @[<hov>Globals:@ %a@]@ Trace:@ @[<hov>%a@]@]"
    gap pp_functions f pp_objects o pp_globals g pp_trace t

let pp_clean_operation pp = function
  | CFunPre { f; base; args; call_type } ->
    fprintf pp "CFunPre(f=%a, base=%a, args=%a, call_type=%a)"
      pp_jsval f pp_jsval base pp_jsval args pp_call_type call_type
  | CFunPost { f; base; args; result; call_type } ->
    fprintf pp "CFunPost(f=%a, base=%a, args=%a, result=%a, call_type=%a"
      pp_jsval f pp_jsval base pp_jsval args
      pp_jsval result pp_call_type call_type
  | CLiteral { value; hasGetterSetter } ->
    fprintf pp "CLiteral(value=%a,hasGetterSetter=%b" pp_jsval value hasGetterSetter
  | CForIn value ->
    fprintf pp "CForIn(%a)" pp_jsval value
  | CDeclare { name; value; declaration_type } ->
    fprintf pp "CDeclare(name=%s, value=%a, declaration_type=%a"
      name pp_jsval value pp_declaration_type declaration_type
  | CGetFieldPre (base, offset) ->
    fprintf pp "CGetFieldPre(base=%a, offset=%s)" pp_jsval base offset
  | CPutFieldPre { base; offset; value } ->
    fprintf pp "CPutFieldPre(base=%a, offset=%s, value=%a)" pp_jsval base offset pp_jsval value
  | CGetField { base; offset; value } ->
    fprintf pp "CGetField(base=%a, offset=%s, value=%a)" pp_jsval base offset pp_jsval value
  | CPutField { base; offset; value } ->
    fprintf pp "CPutField(base=%a, offset=%s, value=%a)" pp_jsval base offset pp_jsval value
  | CRead { name; value } ->
    fprintf pp "CRead(name=%s, value=%a)" name pp_jsval value
  | CWrite { name; lhs; value; isSuccessful } ->
    fprintf pp "CWrite(name=%s, oldValue=%a, newValue=%a, successful=%B)"
      name pp_jsval lhs pp_jsval value isSuccessful
  | CReturn value ->
    fprintf pp "CReturn(value=%a)" pp_jsval value
  | CThrow value ->
    fprintf pp "CThrow(value=%a)" pp_jsval value
  | CWith value ->
    fprintf pp "CWith(value=%a)" pp_jsval value
  | CFunEnter { f; this; args } ->
    fprintf pp "CEnter(f=%a, base=%a, args=%a)"
      pp_jsval f pp_jsval this pp_jsval args
  | CFunExit { ret; exc } ->
    fprintf pp "CExit(value=%a, exception=%a)"
      pp_jsval ret pp_jsval exc
  | CScriptEnter ->
    fprintf pp "CScriptEnter"
  | CScriptExit ->
    fprintf pp "CScriptExit"
  | CScriptExc exc ->
    fprintf pp "CScriptExit(value=%a)" pp_jsval exc
  | CBinary { op; left; right; result } ->
    fprintf pp "CBinary(left=%a, op=%s, right=%a, result=%a)" pp_jsval left op pp_jsval right pp_jsval result
  | CUnary { op; arg; result } ->
    fprintf pp "CUnar(op=%s, arg=%a, result=%a)" op pp_jsval arg pp_jsval result
  | CEndExpression ->
    fprintf pp "CEndExpression"
  | CConditional value ->
    fprintf pp "CConditonal(value=%a)" pp_jsval value

let pp_clean_trace = Fmt.vbox (Fmt.list pp_clean_operation)
let pp_clean_tracefile pp (f, o, t, g, gap) =
  fprintf pp "@[<v>Globals are properties: %b@ @[<hov>%a@]@ @[<hov>%a@]@ @[<hov>Globals:@ %a@]@ Trace:@ @[<hov>%a@]@]"
    gap pp_functions f pp_objects o pp_globals g pp_clean_trace t

let pp_funpre pp ({ f; base; args; call_type }: funpre) =
  Format.fprintf pp "f=%a, base=%a, args=%a, call_type=%a"
    pp_jsval f
    pp_jsval base
    pp_jsval args
    pp_call_type call_type

let pp_synth_stack = Fmt.list (Fmt.option pp_funpre)

let pp_alias_source pp = let open Format in function
    | Argument i -> fprintf pp "declaration of argument %d" i
    | With ref -> fprintf pp "with statement on %a" Reference.pp_versioned_reference ref

let pp_rich_operation pp = let open Format in function
    | RFunPre { f; base; args; call_type } ->
      fprintf pp "RFunPre(f=%a, base=%a, args=%a, call_type=%a)"
        pp_jsval f pp_jsval base pp_jsval args pp_call_type call_type
    | RFunPost { f; base; args; result } ->
      fprintf pp "RFunPost(f=%a, base=%a, args=%a, result=%a)"
        pp_jsval f pp_jsval base pp_jsval args pp_jsval result
    | RLiteral { value; hasGetterSetter } ->
      fprintf pp "RLiteral(value=%a, hasGetterSetter=%B)" pp_jsval value hasGetterSetter;
    | RForIn obj -> fprintf pp "RForIn(value=%a)" pp_jsval obj
    | RLocal { name; ref } ->
      fprintf pp "RLocal(name=%s, ref=%a)" name Reference.pp_versioned_reference ref
    | RCatch { name; ref } ->
      fprintf pp "RCatch(name=%s, ref=%a)" name Reference.pp_versioned_reference ref
    | RAlias { name; source; ref } ->
      fprintf pp "RAlias(name=%s, ref=%a, source=%a)"
        name Reference.pp_versioned_reference ref pp_alias_source source
    | RRead { ref; value } ->
      fprintf pp "RRead(ref=%a, value=%a)"
        Reference.pp_versioned_reference ref pp_jsval value
    | RWrite { ref; value; oldref; success } ->
      fprintf pp "RWrite(ref=%a, value=%a, oldref=%a, success=%B"
        pp_jsval value Reference.pp_versioned_reference ref
        Reference.pp_versioned_reference oldref success
    | RReturn obj -> fprintf pp "RReturn(value=%a)" pp_jsval obj
    | RThrow obj -> fprintf pp "RThrow(value=%a)" pp_jsval obj
    | RWith obj -> fprintf pp "RWith(value=%a)" pp_jsval obj
    | RFunEnter { f; this; args } ->
      fprintf pp "RFunEnter(f=%a, this=%a, args=%a)"
        pp_jsval f pp_jsval this pp_jsval args
    | RFunExit { ret; exc } ->
      fprintf pp "RFunExit(ret=%a, exc=%a)" pp_jsval ret pp_jsval exc
    | RScriptEnter -> pp_print_string pp "RScriptEnter"
    | RScriptExit -> pp_print_string pp "RScriptExit"
    | RScriptExc e -> fprintf pp "RScriptExit(value=%a)" pp_jsval e
    | RBinary { op; left; right; result } ->
      fprintf pp "RBinary(left=%a, op=%s, right=%a, result=%a)"
        pp_jsval left op pp_jsval right pp_jsval result
    | RUnary { op; arg; result } ->
      fprintf pp "RUnary(op=%s, arg=%a, result=%a)" op pp_jsval arg pp_jsval result
    | REndExpression -> pp_print_string pp "REndExpression"
    | RConditional value -> fprintf pp "RConditional(value=%a)" pp_jsval value

let pp_local_facts pp
    { last_arguments; last_update; versions; aliases; points_to } =
  Format.fprintf pp "@[< v >\
                     Last callee-side argument object: %a@ \
                     Last update: %a@ \
                     Versions: @[< hov 2 >%a@]@ \
                     Aliases: @[< hov 2 >%a@]@ \
                     Points-to map: @[< hov 2 >%a@]@ @]"
    (Fmt.option Fmt.int) last_arguments
    (Fmt.option Reference.pp_versioned_reference) last_update
    (Reference.pp_reference_map Format.pp_print_int) versions
    (StringMap.pp (*~entry_sep:(Fmt.const Fmt.string " -> ")*) pp_fieldref) aliases
    Reference.pp_points_to_map points_to

let pp_enriched_trace fmt =
  Fmt.vbox (Fmt.list (Fmt.vbox ~indent:2 (Fmt.append pp_clean_operation (Fmt.prefix Fmt.cut fmt))))
let pp_facts_trace = pp_enriched_trace pp_local_facts

let pp_enriched_tracefile fmt pp (f, o, t, g, gap) =
  Format.fprintf pp
    "@[< v > Globals are properties: %b@ \
     @[< hov >%a@]@ @[< hov >%a@]@ \
     @[< hov > Globals:@ %a@]@ Trace:@ \
     @[< hov >%a@]@]"
    gap pp_functions f pp_objects o pp_globals g (pp_enriched_trace fmt) t
let pp_facts_tracefile = pp_enriched_tracefile pp_local_facts

let dump_facts = ref false
let enable_dump_facts () = dump_facts := true

let pp_rich_operation_with_facts pp (op, facts) =
  if !dump_facts then
    Format.fprintf pp "@[<v 2>%a@.%a@]" pp_rich_operation op pp_local_facts facts
  else
    pp_rich_operation pp op
let pp_rich_trace = Fmt.list pp_rich_operation_with_facts

let pp_rich_tracefile pp
    { funcs; objs; trace; globals; globals_are_properties; points_to } =
  Format.fprintf pp
    "@[< v > Globals are properties: %b@ \
     @[< hov >%a@]@ \
     @[< hov >%a@]@ \
     @[< hov > Globals:@ %a@]@ \
     Trace:@ @[< hov >%a@]@ \
     Points - to:@ %a@]"
    globals_are_properties
    pp_functions funcs
    pp_objects objs
    pp_globals globals
    pp_rich_trace trace
    Reference.pp_points_to_map points_to
