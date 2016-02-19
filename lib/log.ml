let src = Logs.Src.create ~doc:"JSEnrichment library" "jsenrichment"
module Log = (val Logs.src_log src: Logs.LOG)
include Log
let default_setup debugging_mode =
  if debugging_mode then
    Logs.set_level ~all:true (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ())
