open TraceTypes
open Types

let full = ref true

let pp_event_nice pp =
  let open Format in
    function
      | FunPre (_, { f = OFunction(id, _); base; args = OObject argid }) ->
          pp_open_vbox pp 2;
          fprintf pp "FunPre(f=%d, args=%d, base=%a)@ " id argid pp_jsval base
      | FunPost (_, { f = OFunction(id, _); base; args = OObject argid; result }) ->
          pp_close_box pp ();
          fprintf pp "FunPost(f=%d, args=%d, base=%a, result=%a)@ "
            id argid pp_jsval base pp_jsval result
      | FunPre (_, { f; base; args }) ->
          pp_open_vbox pp 2;
          fprintf pp "bad FunPre(f=%a, args=%a, base=%a)@ "
            pp_jsval f pp_jsval args pp_jsval base
      | FunPost (_, { f; base; args; result }) ->
          pp_open_vbox pp 2;
          fprintf pp "bad FunPre(f=%a, args=%a, base=%a, result=%a)@ "
            pp_jsval f pp_jsval args pp_jsval base pp_jsval result
      | FunEnter (_, { f = OFunction(id, _); this; args = OObject argid }) ->
          fprintf pp "FunEnter(f=%d, args=%d, base=%a)@ " id argid pp_jsval this
      | FunEnter (_, { f; this; args }) ->
          fprintf pp "bad FunEnter(f=%a, args=%a, base=%a)@ "
            pp_jsval f pp_jsval args pp_jsval this
      | FunExit (_, { ret; exc }) ->
          fprintf pp "FunExit(ret=%a, exc=%a)@ " pp_jsval ret pp_jsval exc
      | Literal (_, { value }) ->
          if !full then
            fprintf pp "Literal %a@ " pp_jsval value
      | ForIn (_, value) ->
          if !full then
            fprintf pp "ForIn %a@ " pp_jsval value
      | Return (_, value) ->
          if !full then
            fprintf pp "Return %a@ " pp_jsval value
      | Throw (_, value) ->
          if !full then
            fprintf pp "Throw %a@ " pp_jsval value
      | With (_, value) ->
          if !full then
            fprintf pp "With %a@ " pp_jsval value
      | Declare (_, { name; value }) ->
          if !full then
            fprintf pp "Declare %s=%a@ " name pp_jsval value
      | GetFieldPre (_, { base; offset }) ->
          if !full then
            fprintf pp "Get %a.%s (pre)@ " pp_jsval base offset
      | GetField (_, { base; offset; value }) ->
          if !full then
            fprintf pp "Get %a.%s = %a@ " pp_jsval base offset pp_jsval value
      | PutFieldPre (_, { base; offset; value }) ->
          if !full then
            fprintf pp "Put %a.%s := %a (pre)@ " pp_jsval base offset pp_jsval value
      | PutField (_, { base; offset; value }) ->
          if !full then
            fprintf pp "Put %a.%s := %a@ " pp_jsval base offset pp_jsval value
      | Read (_, { name; value }) ->
          if !full then
            fprintf pp "Read %s = %a@ " name pp_jsval value
      | Write (_, { name; value }) ->
          if !full then
            fprintf pp "Write %s := %a@ " name pp_jsval value
      | BinPre (_, { op; left; right }) ->
          if !full then
            fprintf pp "%a %s %a (pre)@ " pp_jsval left op pp_jsval right
      | BinPost (_, { op; left; right; result }) ->
          if !full then
            fprintf pp "%a %s %a = %a@ " pp_jsval left op pp_jsval right pp_jsval result
      | UnaryPre (_, { op; arg }) ->
          if !full then
            fprintf pp "%s %a (pre)@ " op pp_jsval arg
      | UnaryPost (_, { op; arg; result }) ->
          if !full then
            fprintf pp "%s %a = %a@ " op pp_jsval arg pp_jsval result
      | Conditional (iid, value) ->
          if !full then
            fprintf pp "Conditional %d: %a@ " iid pp_jsval value
      | ScriptExc value ->
          fprintf pp "Script exit, exception %a@ " pp_jsval value
      | ScriptExit ->
          fprintf pp "Script exit@ "
      | ScriptEnter ->
          fprintf pp "Script entry@ "
      | EndExpression _ ->
          if !full then
            fprintf pp "end of expression@ "

let () =
  let files = ref []
  and debug = ref false in
    Arg.parse [
      ("-D", Arg.Set debug, "Debugging mode");
      ("-s", Arg.Clear full, "Shortened mode")
    ]
      (fun file -> files := !files @ [file])
      "ppcleantrace [-D] files";
    Log.default_setup !debug;
    List.iter (fun file -> file
                 |> Trace.read_tracefile
                 |> fun (_, _, trace, _, _) ->
                    Format.printf "@[<v>%a@]@." (Fmt.list ~sep:(fun _ _ -> ()) pp_event_nice) trace)
      !files
