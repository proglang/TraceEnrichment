open TraceTypes
val calculate_rich_tracefile : facts_tracefile -> rich_tracefile
val calculate_rich_stream : Types.initials -> facts_stream -> rich_stream

val tracefile_to_rich_tracefile : tracefile -> rich_tracefile
val trace_stream_to_rich_stream : Types.initials -> raw_stream -> rich_stream
