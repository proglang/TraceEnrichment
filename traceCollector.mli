exception InstrumentationError
val instrument : string -> string -> unit Lwt.t
val trace_collection_server :
  (init_data:string -> id:string -> (string -> unit) option Lwt.t) -> unit Lwt.t
val one_shot_server: (Reference.initials -> TraceTypes.raw_stream -> 'a Lwt.t) -> 'a Lwt.t
val generic_server:
  (string -> Reference.initials -> TraceTypes.raw_stream -> unit) ->
  unit Lwt.t

