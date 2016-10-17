let header = {html|
<html><head>
   <title>Trace</title>
   <style type="text/css">
.kw { font-weight: bold }
.string { font-family: monospace; max-width: 50% }
   </style>
</head>
<body>
<table>
<col width="10%"/><col width="60%"/><col width="30%"/>
<tr><th>Where</th><th>Event</th><th>Facts</th></tr>
|html}

let footer = {html|
</table>
</body></html>
|html}

let html_escape =
  BatString.replace_chars (function
                             | '<' -> "&lt;"
                             | '>' -> "&gt;"
                             | '&' -> "&aamp;"
                             | c -> BatString.of_char c)

let fmt_jsval pp =
  let open TypesJS in
  let open Fmt in function
    OUndefined -> string pp "<span class=\"kw\">undefined</span>"
  | ONull -> string pp "<span class=\"kw\">null</span>"
  | OBoolean true -> string pp "<span class=\"kw\">true</span>"
  | OBoolean false -> string pp "<span class=\"kw\">false</span>"
  | ONumberInt n -> int pp n
  | ONumberFloat x -> float pp x
  | OString s -> pf pp "<span class=\"string\">\"%s\"</span>" (html_escape s)
  | OSymbol s -> pf  pp"symbol:%s" s
  | OFunction (id, fid) -> pf pp "<a href=\"f%d.%d\">function(%d, %d)</a>" id fid id fid
  | OObject id -> pf  pp "<a href=\"o%d\">object(%d)</a>" id id
  | OOther (ty, id) -> pf  pp "<a href=\"x%s.%d\">%s(%d)" ty id ty id



let fmt_call_type pp ct =
  let open TraceTypes in
    Fmt.string pp (match ct with
                     | Function -> "function"
                     | Method -> "method"
                     | Constructor -> "constructor")

let fmt_funpre pp: TraceTypes.funpre -> 'a =
  let open TraceTypes in
    fun { f; base; args; call_type } ->
      Fmt.pf pp "f=%a, base=%a, args=%a, call_type=%a"
        fmt_jsval f
        fmt_jsval base
        fmt_jsval args
        fmt_call_type call_type

let fmt_funpost pp =
  let open TraceTypes in
    fun { f; base; args; call_type; result } ->
      Fmt.pf pp "f=%a, base=%a, args=%a, call_type=%a, result=%a"
        fmt_jsval f
        fmt_jsval base
        fmt_jsval args
        fmt_call_type call_type
        fmt_jsval result

let inner_fmt_reference pp =
  let open Reference in
  let open TypesJS in
  let open Fmt in function
    | Field (Object 0, name) ->
        string pp name
    | Field (obj, prop) ->
        pair ~sep:(const string ".")
           (using objectid_to_jsval fmt_jsval)
           string
          pp
          (obj, prop)
    | Variable (Global, name) ->
        string pp name
    | Variable (Local i, name) ->
        pf pp "local %d:%s" i name

let fmt_reference pp = inner_fmt_reference pp

let fmt_versioned_reference =
  let open Fmt in
    pair ~sep:(const string ":") inner_fmt_reference int

let fmt_literal pp: TraceTypes.literal -> _ =
  let open TraceTypes in
    fun ({ value; hasGetterSetter }: TraceTypes.literal) ->
      if hasGetterSetter then
        Fmt.pf pp "%a (with getter and setter)" fmt_jsval value
      else
        fmt_jsval pp value

let fmt_local pp =
  let open TraceTypes in
    fun ({ name; ref }: TraceTypes.local) ->
      Fmt.pf pp "%s &rArr; %a" name fmt_versioned_reference ref

let fmt_alias pp =
  let open TraceTypes in
    fun ({ name; ref; source }: TraceTypes.alias) ->
      Fmt.pf pp "%s &rArr; %a (%a)"
        name fmt_versioned_reference ref
        (fun pp -> function
           | Argument i -> Fmt.pf pp "argument %d" i
           | With ref -> Fmt.pf pp "with on %a" fmt_versioned_reference ref)
        source

let fmt_read pp =
  let open TraceTypes in
    fun ({ ref; orig_ref; value; isComputed }: rread) ->
      if fst ref = orig_ref then
        Fmt.pf pp "%s%a &rArr; %a"
          (if isComputed then "computed " else "")
          fmt_versioned_reference ref
          fmt_jsval value
      else 
        Fmt.pf pp "%s%a (from %a) &rArr; %a"
          (if isComputed then "computed " else "")
          fmt_versioned_reference ref
          fmt_reference orig_ref
          fmt_jsval value

let fmt_write pp =
  let open TraceTypes in
    fun ({ ref; orig_ref; value; isComputed; oldref; success }: rwrite) ->
      if fst ref = orig_ref then
        Fmt.pf pp "%s%a &rArr; %a, success: %b, new reference %a"
          (if isComputed then "computed " else "")
          fmt_versioned_reference oldref
          fmt_jsval value
          success
          fmt_versioned_reference ref
      else 
        Fmt.pf pp "%s%a (from %a) &rArr; %a, success: %b, new reference %a"
          (if isComputed then "computed " else "")
          fmt_versioned_reference oldref
          fmt_reference orig_ref
          fmt_jsval value
          success
          fmt_versioned_reference ref

let fmt_enter pp =
  let open TraceTypes in
    fun ({ f; this; args }) ->
      Fmt.pf pp "f=%a; this=%a; args=%a"
        fmt_jsval f
        fmt_jsval this
        fmt_jsval args

let fmt_exit pp =
  let open TraceTypes in
  let open Fmt in
    function
      | { ret; exc = TypesJS.OUndefined } ->
          pf pp "regular: ret=%a" fmt_jsval ret
      | { ret = TypesJS.OUndefined; exc } ->
          pf pp "exception: exc=%a" fmt_jsval exc
      | { ret; exc } ->
          pf pp "ret=%a, exc=%a" fmt_jsval ret fmt_jsval exc

let fmt_unary pp =
  let open TraceTypes in
    fun { op; arg; result } ->
      Fmt.pf pp "%s %a &rArr; %a" op fmt_jsval arg fmt_jsval result

let fmt_binary pp =
  let open TraceTypes in
    fun { op; left; right; result } ->
      Fmt.pf pp "%a %s %a &rArr; %a"
        fmt_jsval left op fmt_jsval right fmt_jsval result
                          
let fmt_location = TypesJS.pp_location

let fmt_operation pp =
  let open TraceTypes in
  let open Fmt in function
      RFunPre fp -> pf pp "RFunPre(%a)" fmt_funpre fp
    | RFunPost fp -> pf pp "RFunPost(%a)" fmt_funpost fp
    | RLiteral li -> pf pp "RLiteral(%a)" fmt_literal li
    | RForIn x -> pf pp "RForIn(%a)" fmt_jsval x
    | RLocal lo -> pf pp "RLocal(%a)" fmt_local lo
    | RCatch lo -> pf pp "RLocal(%a)" fmt_local lo
    | RAlias al -> pf pp "RAlias(%a)" fmt_alias al
    | RRead re -> pf pp "RRead(%a)" fmt_read re
    | RWrite re -> pf pp "RWrite(%a)" fmt_write re
    | RReturn x -> pf pp "RReturn(%a)" fmt_jsval x
    | RThrow x -> pf pp "RThrow(%a)" fmt_jsval x
    | RWith x -> pf pp "RWith(%a)" fmt_jsval x
    | RFunEnter fe -> pf pp "RFunEnter(%a)" fmt_enter fe
    | RFunExit fe -> pf pp "RFunExit(%a)" fmt_exit fe
    | RScriptEnter -> string pp "RScriptEnter"
    | RScriptExit -> string pp "RScriptExit"
    | RScriptExc x -> pf pp "RScriptExc(%a)" fmt_jsval x
    | RBinary bi -> pf pp "RBinary(%a)" fmt_binary bi
    | RUnary un -> pf pp "RUnary(%a)" fmt_unary un
    | REndExpression -> string pp "REndExpression"
    | RConditional (Some loc, x) ->
        pf pp "RConditional(%a) @ %a"
          fmt_jsval x
          fmt_location loc
    | RConditional (None, x) ->
        pf pp "RConditional(%a)"
          fmt_jsval x

let fmt_sid pp =
  let open Fmt in function
    | (sid, tsid, true) when sid = tsid ->
        pf pp "event handler %d, top" sid
    | (sid, tsid, false) when sid = tsid ->
        pf pp "script %d, top" sid
    | (sid, tsid, true) ->
        pf pp "event handler %d, script %d" tsid sid
    | (sid, tsid, false) ->
        pf pp "script %d, script %d" tsid sid

let to_html =
  let open TraceTypes in
  let open Fmt in
    fun (op, { versions; points_to; names; sid; tsid; in_event_handler }) ->
      strf "<tr><td>%a</td><td>%a</td><td>(left out for now)</td></tr>"
        fmt_sid (sid, tsid, in_event_handler)
        fmt_operation op


module TestStreamStrategy: TraceStreamServer.STREAMSTRATEGY = struct
  open Lwt
  type t = string Lwt_stream.t
  let stream_setup id initials raw_stream: t =
    Log.info (fun m -> m "Setting up stream");
    let body =
      RichTrace.trace_stream_to_rich_stream initials raw_stream |>
        Lwt_stream.map to_html
    in Lwt_stream.concat (Lwt_stream.of_list
                            [Lwt_stream.of_list [header];
                             body;
                             Lwt_stream.of_list [footer]])

  let stream_dump _ (html: t) _ _ =
    Log.info (fun m -> m "Preparing for dump");
    TraceCollector.reply_stream "text/html" html

  let handlers_global = []
  let handlers_local = [
    ("pretty", let open Cohttp.Code in `GET),
    ("Pretty-print trace", stream_dump)
  ]
end

module Server = TraceStreamServer.TraceStreamServer(TestStreamStrategy)

let () =
  Arg.parse Config.args (fun _ -> failwith "Unexpected argument")
    "traceStreamServerTest [options]";
  Log.default_setup true;
  Server.run_server ()

