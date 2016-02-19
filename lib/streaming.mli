(** Generic implementation of trace handling.
 
  The implementation of js-enrichment is factored
  over the underlying representation of the trace.
  It can deal both with offline traces, represented
  as a list, and online traces, represented as a stream.

  To facilitate this, a set of standard transformations
  on these representations is provided, so that all
  required steps can be written once in a generic way,
  and then implemented using functors. *)

module Stream = Lwt_stream

(** The required transformers for sequences. *)
module type Transformers = sig
  (**
    The sequence may life inside a monad (e.g., for [Lwt_stream]). This
    is reflected in the type of some transformers. *)
  type 'a monad
  (** The sequence type. *)
  type 'a sequence

  (** [map f s] transforms the sequence [s] to a new sequence
    by applying [f] to each element of the sequence in turn,
    and constructing a sequence from the results.
    
    For a sequence [x_1, ..., x_n], the result is
    [f x_1, ..., f x_n].
    *)
  val map: ('a -> 'b) -> 'a sequence -> 'b sequence
  (** [map_state a f s] transforms the sequence [s] to a new sequence
    by applying [f a x] to each element [x] of the sequence in turn,
    updating [a] from the result and adding the new element to the
    output sequence.

    For a sequence [x_1, ..., x_n] and state [a_1],
    if [f a_i x_i = (y_i, a_{i+1})], then the
    output sequence is [y_1, ..., y_n].
    *)
  val map_state: 's -> ('s -> 'a -> 'b * 's) -> 'a sequence -> 'b sequence
  (** [map_list f s] transforms the sequence [s] to a new sequence
    by applying [f] to each element of the sequence in turn,
    and constructing a sequence from the result sequences.

    For a sequence [x_1, ..., x_n], if [f x_i = (y_{i,1}, ..., y_{i,k_i})],
    then the result is [y_{1,1}, y_{1,2}, ..., y_{1,k_1}, ..., y_{n,k_n}]
    *)
  val map_list: ('a -> 'b list) -> 'a sequence -> 'b sequence
  (** [map_list_state a f s] transforms the sequence [s] to a new sequence
    by applying [f a x] to each element [x] of the sequence in turn,
    constructing a sequence from the result sequences and updating [a] each
    step.

    For a sequence [x_1, ..., x_n], if [f a_i x_i = (a_{i+1}, (y_{i,1}, ..., y_{i,k_i}))],
    then the result is [y_{1,1}, y_{1,2}, ..., y_{1,k_1}, ..., y_{n,k_n}]
    *)
  val map_list_state: 's -> ('s -> 'a -> 'b list * 's) -> 'a sequence -> 'b sequence
  (** [validation f a s] copies [s] from input to output, and also performs
    validation on it.

    In particular, validation is performed element-wise, with the value of
    [a] being updated: If [f a_i x_i = a_{i+1}], then applying
    [validation f a s] to [s = (x_1, ..., x_n)] calls
    [f a x_1], [f a_2 x_2], [f a_3 x_3] and so on. *)
  val validation: ('a -> 's -> 's) -> 's -> 'a sequence -> 'a sequence
  (** [collect f s a] collects information from a trace. For a trace
    [s = (x_1, ..., x_n)], [collect f s a = f x_n (... (f x_1 a) ...)].
    *)
  val collect: ('a -> 'b -> 'b) -> 'a sequence -> 'b -> 'b monad
end;;

(** Implementation of sequence transformers for Lwt streams. *)
module StreamTransformers: Transformers
  with type 'a monad = 'a Lwt.t and type 'a sequence = 'a Stream.t;;
(** Implementation of sequence transformers for lists. *)
module ListTransformers: Transformers
  with type 'a monad = 'a and type 'a sequence = 'a list;;

