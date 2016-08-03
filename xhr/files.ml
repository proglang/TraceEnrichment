(** Some file handling functions that are missing in stdlib and batteries. *)

(* stolen from the OCaml standard library *)
let prng = lazy(Random.State.make_self_init ())

(** Create a temporary filename *)
let temp_file_name temp_dir prefix suffix =
  let rnd = (Random.State.bits (Lazy.force prng)) land 0xFFFFFF in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

(** Create a temporary directory, using LWT functionality. *)
let temp_dir_lwt ?(temp_dir = Filename.get_temp_dir_name ()) prefix suffix =
  let rec make n =
    try%lwt
      let dir = temp_file_name temp_dir prefix suffix in
      let%lwt _ = Lwt_unix.mkdir dir 0o700 in
        Lwt.return dir
    with Unix.Unix_error (Unix.EEXIST, _, _) as e -> if n > 1000 then raise e else make (n+1)
  in make 0 

(** Remove a subdirectory hierarchy. *)
let rm_rf_lwt base =
  Lwt_process.exec ("/bin/rm", [| "rm"; "-rf"; base |])
