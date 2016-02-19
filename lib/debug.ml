let debug = ref false
let validate = ref false
let debug_chan = ref Format.err_formatter
let redirect_to_channel chan =
  debug_chan := Format.formatter_of_out_channel chan
let is_debug () = !debug
let is_validate () = !validate
let enable_validate () = validate := true
let disable_validate () = validate := false
let enable () = debug := true; enable_validate ()
let disable () = debug := false

let debug (fmt: ('a, Format.formatter, unit) format) =
  if !debug then
    Format.fprintf !debug_chan fmt
  else
    Format.ifprintf !debug_chan fmt
