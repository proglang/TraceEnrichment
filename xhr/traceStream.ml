open Trace
open TraceTypes
open Types

type items =
  | ItemFunction of int * funcspec
  | ItemFunctionUninstrumented of int * string
  | ItemObject of int * objectspec
  | ItemStep of event
  | ItemEnd
  | ItemStart

exception InvalidItem of string

let parse_item json =
  let open Yojson.Basic in
  match Yojson.Basic.Util.to_list json with
    | [`String "function"; `Int id; spec] ->
        ItemFunction (id, parse_funcspec spec)
    | [`String "object"; `Int id; spec] ->
        ItemObject (id, parse_objectspec spec)
    | [`String "step"; json] ->
        ItemStep (parse_operation json)
    | [`String "function-uninstrumented"; `Int id; `String code] ->
        ItemFunctionUninstrumented (id, code)
    | [`String "end" ] ->
        ItemEnd
    | [`String "start" ] ->
        ItemStart
    | _ ->
        raise (InvalidItem ("Bad item: " ^ Yojson.Basic.to_string json))

let rec extract handler = function
  | x::l ->
      if handler x then
        extract handler l
      else
        x :: extract handler l
  | [] -> []

let rec handle_end = function
  | ItemEnd :: _ -> ([], true)
  | item :: items -> let (items, at_end) = handle_end items in (item::items, at_end)
  | [] -> ([], false)

let function_handler initials = function
  | ItemFunction (id, spec) ->
      let open Reference in
        BatDynArray.insert initials.functions id spec;
        true
  | _ -> false

let object_handler initials = function
  | ItemObject (id, spec) ->
      let open Reference in
        BatDynArray.insert initials.objects id spec;
        true
  | _ -> false

let function_uninstrumented_handler initials = function
  | ItemFunctionUninstrumented (id, code) ->
      begin
        let open Reference in
          match BatDynArray.get initials.functions id with
            | Instrumented ins ->
                BatDynArray.set initials.functions id
                  (Uninstrumented (ins, code))
            | Uninstrumented (ins, _) ->
                Log.err (fun m -> m "Adding uninstrumented code to a function that already has this.");
                BatDynArray.set initials.functions id
                  (Uninstrumented (ins, code))
            | External _ -> raise (InvalidItem "functionUninstrumented for external")
      end; true
  | _ -> false

let rec handle_start initials = function
  | ItemStart :: items ->
      lookup_functions initials;
      items
  | item :: items -> item :: handle_start initials items
  | [] -> []

let parse_packet initials event_push json_string =
  Log.debug (fun m -> m "Handling data packet %s" json_string);
  let items =
    Yojson.Basic.from_string json_string
    |> Yojson.Basic.Util.convert_each parse_item
  in let (operations, at_end) =
    items
    |> extract (function_handler initials)
    |> extract (function_uninstrumented_handler initials)
    |> extract (object_handler initials)
    |> handle_start initials
    |> handle_end 
  in
    Log.debug (fun m -> m "Extracted trace operations. At end: %b, %d operations"
                          at_end (List.length operations));
  let trace =
    List.map (function ItemStep op -> op | _ -> failwith "Only ItemStep can happen!")
      operations
  in
    Log.debug (fun m -> m "Prepare event feeding");
    List.iter (fun op -> event_push (Some op)) trace;
    Log.debug (fun m -> m "Finished event feeding");
    if at_end then event_push None;
    Log.debug (fun m -> m "At-end handling")


let parse_setup_packet json_string =
  match Yojson.Basic.from_string json_string |> Yojson.Basic.Util.to_list with
    | [ `Bool globals_are_properties; `Assoc globals_json ] ->
        let globals = List.fold_left (fun globals (name, val_json) ->
                                        StringMap.add name (parse_jsval val_json) globals)
                        StringMap.empty globals_json
        in let open Reference in
        let initials =
          { globals_are_properties; globals;
            objects = BatDynArray.create ();
            functions = BatDynArray.create ();
            function_call = OUndefined;
            function_apply = OUndefined;
            function_eval = OUndefined;
            function_constructor = OUndefined;
          }
        in let (stream, push) = Lwt_stream.create ()
        in (initials, stream, parse_packet initials push)
    | _ -> raise (InvalidItem ("Bad setup packet: " ^ json_string))

