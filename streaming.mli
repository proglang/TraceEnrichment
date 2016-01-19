module Stream = Lwt_stream

module type Transformers = sig
  type 'a monad
  type 'a sequence

  val return: 'a -> 'a monad
  val bind: 'a monad -> ('a -> 'b monad) -> 'b monad

  val peek: 'a sequence -> 'a option monad
  val try_read: ('a -> bool) -> 'a sequence -> 'a option monad
  val map_state: 's -> ('s -> 'a -> 'b * 's) -> 'a sequence -> 'b sequence
  val map_list_state: 's -> ('s -> 'a -> 'b list * 's) -> 'a sequence -> 'b sequence
  val observe: ('a -> unit) -> 'a sequence -> 'a sequence
  val map: ('a -> 'b) -> 'a sequence -> 'b sequence
  val map_list: ('a -> 'b list) -> 'a sequence -> 'b sequence
  val validation: ('a -> 's -> 's) -> 's -> 'a sequence -> 'a sequence
end;;

module StreamTransformers: Transformers
  with type 'a monad = 'a Lwt.t and type 'a sequence = 'a Stream.t;;
module ListTransformers: Transformers
  with type 'a monad = 'a and type 'a sequence = 'a list;;

