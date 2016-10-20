(** Interface with Jalangi2. *)
(** Instrumentation failed. *)
exception InstrumentationError
(** Perform instrumentation.
   
  [jalangi2_instrument strategy filenames outdir] instruments the
  files in [filenames] using the generic trace analysis, using
  trace output strategy [strategy], and puts the results into
  [outdir]. *)
val jalangi2_instrument : string -> string list -> string -> unit Lwt.t
(**
  A complete solution for instrumenting a JavaScript file for browser-based
  trace generation.

  [basename] gives a base name for the instrumented JavaScript file and
  the HTML page that drives the analysis.

  [providejs] gets called as [providejs jsfile], where [jsfile] is the proposed
  filename for the uninstrumented JavaScript file. 

  The function performs the instrumentation and returns the base name of
  instrumented JavaScript file and driver page, i.e., if it returns
  "/path/to/xyz", then the instrumented JS file can be found in
  "/path/to/xyz.js" and the driver page in "/path/to/xyz.html".
  *)
val instrument_for_browser :
  ?basename:string ->
  providejs:(string -> unit Lwt.t) -> string Lwt.t

(** Return the path containing the instrumented files. *)
val get_instrumented_dir: unit -> string Lwt.t

(** Clean up temporary directories.
  
  This code is meant for program shut-down cleanup, and performs
  operations synchronously. *)
val clean_up: unit -> unit

(**
  A complete solution for instrumenting a whole web page for browser-based
  analysis.

  The function gets calls as [instrument_page headers uri], and instruments
  the page at the given URI.
  It returs the path to the instrumented HTML file.
  *)
val instrument_page : Cohttp.Header.t -> string -> string Lwt.t
