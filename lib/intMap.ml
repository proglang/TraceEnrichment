(** Maps with integers as keys. Just an instance of [BatMap]. *)
include ExtMap.Make(struct
                      include BatInt
                      let pp = Fmt.int
                    end)

