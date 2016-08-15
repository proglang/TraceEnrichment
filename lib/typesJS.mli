(** Basic types used everywhere in js-enrichment. *)

(** {1 JavaScript values} *)
(** [jsval] represents a value, either directly or as a reference to the object and/or function tables.  *)
type jsval =
    OUndefined
  | ONull
  | OBoolean of bool
  | ONumberInt of int
  | ONumberFloat of float
  | OString of string
  | OSymbol of string
  | OFunction of int * int
  (** [OFunction(id, fid)] stands for a function belonging to object [id], with function id [fid]. *)
  | OObject of int
  (** [OObject id] stands for the object with object id [id]. *)
  | OOther of string * int
  (** [OOther (ty, id)] stands for a special object with type [ty] and object id [id]. *)

(** Boilerplate code. *)
val equal_jsval : jsval -> jsval -> bool
val compare_jsval : jsval -> jsval -> int
val pp_jsval : jsval Fmt.t

(** This exception is thrown whenever an operation is performed on a value
    that is not an object (i.e., not one of [OFunction], [OObject] or [OOther]),
    while the function is defined only on objects. *)
exception NotAnObject
(** Get the identifier of the underlying object of the value. May throw [NotAnObject]. *)
val get_object : jsval -> int
(** Try to get the identifier of the underlying object.
    Exceptionless variant of [get_object] *)
val try_get_object : jsval -> int option
(** Check if the given value is a base value, i.e., it has no underlying object. *)
val is_base : jsval -> bool

(** {1 Object references} *)
(** [objectid] represents a value that has an underlying object. *)
type objectid = Object of int | Function of int * int | Other of string * int

(** Boilerplate code. *)
val pp_objectid : objectid Fmt.t
val show_objectid : objectid -> string
val compare_objectid : objectid -> objectid -> int
val equal_objectid : objectid -> objectid -> bool

(** Maps on objectids. *)
module ObjectIDMap: ExtMap.S with type key = objectid

(** Get the object identifier of the value. *)
val get_object_id : objectid -> int
(** Transform an [objectid] to the corresponding [jsval].
    This is a straightforward injection. *)
val objectid_to_jsval : objectid -> jsval
(** Transform a [jsval] to the corresponding [objectid]. This is a partial
    function, which will throw [NotAnObject] if not defined. *)
val objectid_of_jsval : jsval -> objectid
(** Try to transform a [jsval] to the corresponding [objectid].
    Exceptionless variant of [objectid_of_jsval]. *)
val try_objectid_of_jsval : jsval -> objectid option

(** {1 Object properties} *)
(** Reference to a property of an object, given by the corresponding object value
    and the property name. *)
type fieldref = objectid * string
(** Boilerplate code. *)
val pp_fieldref : fieldref Fmt.t
val show_fieldref : fieldref -> string
val compare_fieldref : fieldref -> fieldref -> int
val equal_fieldref : fieldref -> fieldref -> bool

(** The description of a field in an object. This record reflects
   the structure of the Property Descriptor described in ECMAScript 6. *)
type fieldspec = {
  value : jsval;
  writable : bool;
  get : jsval option;
  set : jsval option;
  enumerable : bool;
  configurable : bool;
}
(** Boilerplate code. *)
val pp_fieldspec : fieldspec Fmt.t
val equal_fieldspec : fieldspec -> fieldspec -> bool
val compare_fieldspec : fieldspec -> fieldspec -> int
val make_fieldspec :
  value:jsval ->
  ?writable:bool ->
  ?get:jsval ->
  ?set:jsval -> ?enumerable:bool -> ?configurable:bool -> unit -> fieldspec

(** {1 Description of objects by properties} *)
(** An object can be described by a description of all its properties.

   Note that complete knowledge of object fields is not always available;
   this comes up in [MatchObjects] in JSTruthiness. *)
type objectspec = fieldspec StringMap.t
(** Boilerplate code. *)
val pp_objectspec :
  objectspec Fmt.t
val show_objectspec : objectspec -> string
val equal_objectspec : objectspec -> objectspec -> bool

(** {1 The object array} *)
(** Description of all object initial states in the program. *)
type objects = objectspec BatDynArray.t
(** Boilerplate code. *)
val pp_objects : objects Fmt.t
val equal_objects :
  objectspec BatDynArray.t -> objectspec BatDynArray.t -> bool

(** {1 Description of JavaScript functions} *)
(** Description of a Javascript function. *)
type funcspec = 
  | ReflectedCode of string (** Possibly insturmented JavaScript function. *)
  | OrigCode of string * string
    (** Function that has been instrumented by Jalangi.
        [ReflectedCode (ins, unins)] contains the instrumented
        code in [ins] and the uninstrumented code in [unins]. *)
  | External of int (** External function, implemented in native code *)
(** Boilerplate code. *)
val pp_funcspec : funcspec Fmt.t
val show_funcspec : funcspec -> string
val compare_funcspec : funcspec -> funcspec -> int
val equal_funcspec : funcspec -> funcspec -> bool

(** {1 The function array} *)
(** Description of all Javascript functions encountered in a trace. *)
type functions = funcspec BatDynArray.t
(** Boilerplate code. *)
val pp_functions : functions Fmt.t
val equal_functions :
  funcspec BatDynArray.t -> funcspec BatDynArray.t -> bool

(** {1 The global map} *)
(** The values of all (known) global variables. *)
type globals = jsval StringMap.t
(** Boilerplate code *)
val pp_globals : globals Fmt.t
val show_globals : globals -> string
val equal_globals : globals -> globals -> bool

(** {1 IID maps} *)
type location = {
  first_line: int;
  last_line: int;
  first_char: int;
  last_char: int
}
val pp_location : location Fmt.t
val compare_location : location -> location -> int
type iidmap = location CCIntMap.t CCIntMap.t
val pp_iidmap : iidmap Fmt.t

(** {1 The initial states} *)
(**
  Initial state data, containing globals, object descriptions
  and function descriptions. This is used in streaming.

  Nota bene: functions and objects are, in fact, mutable internally. *)
type initials = {
  functions : functions;
  objects : objects;
  globals : globals;
  globals_are_properties : bool;
  mutable iids: iidmap;
  mutable function_apply: jsval;
  mutable function_call: jsval;
  mutable function_constructor: jsval;
  mutable function_eval: jsval;
  mutable object_getPrototypeOf: jsval;
  mutable object_setPrototypeOf: jsval;
  mutable reflect_getPrototypeOf: jsval;
  mutable reflect_setPrototypeOf: jsval;
}

(** Look up objects and functions *)
exception ObjectNotFound of objectid * string
val lookup_object: ?required:bool -> objects -> jsval -> string -> jsval
val lookup_functions: initials -> unit

(** Boilerplate code. *)
val pp_initials : initials Fmt.t
val show_initials : initials -> string
val equal_initials : initials -> initials -> bool


