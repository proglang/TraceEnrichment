open Lwt
module Stream = Lwt_stream

module type Transformers = sig
  type 'a monad
  type 'a sequence

  val map: ('a -> 'b) -> 'a sequence -> 'b sequence
  val map_state: 's -> ('s -> 'a -> 'b * 's) -> 'a sequence -> 'b sequence
  val map_list: ('a -> 'b list) -> 'a sequence -> 'b sequence
  val map_list_state: 's -> ('s -> 'a -> 'b list * 's) -> 'a sequence -> 'b sequence
  val validation: ('a -> 's -> 's) -> 's -> 'a sequence -> 'a sequence
  val collect: ('a -> 'b -> 'b) -> 'a sequence -> 'b -> 'b monad
end;;

module StreamTransformers = struct
  type 'a monad = 'a Lwt.t
  type 'a sequence = 'a Stream.t

  let map = Stream.map

  let map_list = Stream.map_list

  let map_state init f stream =
    let s = ref init in
    Stream.map (fun x -> let (y, s') = f !s x in s := s'; y) stream

  let map_list_state init f stream =
    let s = ref init in
    Stream.map_list (fun x -> let (y, s') = f !s x in s := s'; y) stream

  let validation f init str =
    Stream.fold f (Stream.clone str) init |> ignore; str

  let collect f init str =
    Stream.fold f init str

end;;

module ListTransformers = struct
  type 'a monad = 'a
  type 'a sequence = 'a list

  let map_state init f =
    let rec ms s = function
      | [] -> []
      | x::l -> let (y, s') = f s x in y :: ms s' l
    in ms init

  let map_list_state init f =
    let rec ms s = function
      | [] -> []
      | x::l -> let (y, s') = f s x in y @ ms s' l
    in ms init

  let map = List.map
  let rec map_list f = function
    | [] -> []
    | x::l -> f x @ map_list f l

  let validation f init l =
    List.fold_left (fun s o -> f o s) init l |> ignore; l

  let collect f init l =
    List.fold_left (fun s o -> f o s) l init
end

