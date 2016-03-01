let src = Logs.Src.create ~doc:"JSEnrichment library" "jsenrichment"
module Log = (val Logs.src_log src: Logs.LOG)
include Log
let default_setup debugging_mode =
  if debugging_mode then begin
    Logs.set_level ~all:true (Some Logs.Debug);
    Format.set_margin 132
  end;
  Logs.set_reporter (Logs_fmt.reporter ())
