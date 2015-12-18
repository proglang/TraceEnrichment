open Lwt
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
end;;

module StreamTransformers = struct
  type 'a monad = 'a Lwt.t
  type 'a sequence = 'a Stream.t

  let return = Lwt.return
  let bind = Lwt.bind

  let peek = Stream.peek

  let try_read cond str =
    match%lwt Stream.peek str with
    | None -> Lwt.return_none
    | Some x ->
      if cond x then
        Lwt.return_some x
      else
        Lwt.return_none

  let map_state init f stream =
    let s = ref init in
    Stream.map (fun x -> let (y, s') = f !s x in s := s'; y) stream

  let map_list_state init f stream =
    let s = ref init in
    Stream.map_list (fun x -> let (y, s') = f !s x in s := s'; y) stream

  let observe f stream = Stream.map (fun x -> f x; x) stream

  let map = Stream.map

  let map_list = Stream.map_list
end;;

module ListTransformers = struct
  type 'a monad = 'a
  type 'a sequence = 'a list

  let return x = x
  let bind x f = f x

  let peek = function
    | x::_ -> Some x
    | [] -> None

  let try_read cond = function
    | [] -> None
    | x::_ -> if cond x then Some x else None

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

  let observe f l = List.iter (fun x -> ignore (f x)) l; l

  let map = List.map
  let rec map_list f = function
    | [] -> []
    | x::l -> f x @ map_list f l
end

