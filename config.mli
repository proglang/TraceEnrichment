(** Configuration parameters for trace collection. *)

(** Argument descriptions for the configuration parameters.
  This is in the format the the [Arg] library expects. *)
val args : (string * Arg.spec * string) list
(** Get the path to the node.js interpreter to use. *)
val get_node_path : unit -> string
(** Get the path to the Jalangi2 implementation to use. *)
val get_jalangi2_path : unit -> string
(** Get the path to the analysis script. *)                                  
val get_analysis_script_path : unit -> string
(** Get the binding address of the trace collection XHR server. *)
val get_xhr_server_address : unit -> string
(** Get the port of the trace collection XHR server. *)
val get_xhr_server_port : unit -> int
