type 'a extarray
val get : 'a extarray -> int -> 'a
val make : int -> 'a -> 'a extarray
val append : 'a extarray -> 'a -> unit
val to_array : 'a extarray -> 'a array
val of_array : 'a array -> 'a extarray
val to_list : 'a extarray -> 'a list
val of_list : 'a list -> 'a extarray
val iteri : (int -> 'a -> unit) -> 'a extarray -> unit
val foldi : (int -> 'a -> 'b -> 'b) -> 'a extarray -> 'b -> 'b
val length : 'a extarray -> int
