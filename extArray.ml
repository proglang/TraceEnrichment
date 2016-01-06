type 'a extarray = {
  mutable data: 'a array;
  mutable size: int
}

let length a = a.size

let get a i =
  if i < a.size then a.data.(i) else invalid_arg "index out of bounds"

let to_next_power_of_2 n =
  if n = 0 then 1 else
    (* This gives the scaling exponent of the number's float representation *)
    let (_, exp) = frexp (float_of_int (n-1))
    (* Calculate the next-bigger power of two. *)
    in 1 lsl exp

let to_next_power_of_2 n =
  let p = to_next_power_of_2 n in
    assert (p >= n); p

let make n v = { data = Array.make (to_next_power_of_2 n) v; size = n }

let append a v =
  let i = Array.length a.data in
    assert (i >= 0);
  if a.size < i then begin
    a.data.(a.size) <- v;
    a.size <- a.size + 1
  end else if i = 0 then begin
    a.data <- Array.make 1 v;
    a.size <- 1
  end else begin
    a.data <- Array.append a.data (Array.make i v);
    a.size <- a.size + 1
  end

let to_array a = Array.sub a.data 0 a.size
let of_array a =
  let n = Array.length a in
  if n = 0 then
    { data = [| |]; size = 0 }
  else
    { data = Array.append a (Array.make (to_next_power_of_2 n - n) a.(0));
      size = n }
let to_list a = a |> to_array |> Array.to_list
let of_list l = l |> Array.of_list |> of_array

let iteri f a =
  for i = 0 to a.size do
    f i a.data.(i)
  done

let foldi f a b =
  let acc = ref b in
    for i = 0 to a.size do
      acc := f i a.data.(i) !acc
    done;
    !acc
