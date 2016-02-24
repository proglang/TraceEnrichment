(** Maps with integer pairs as keys. Essentially, just an instance of [BatMap]. *)
include ExtMap.Make(struct
                      include BatTuple.Tuple2.Comp(BatInt)(BatInt)
                      let pp = Fmt.pair Fmt.int Fmt.int
                    end)

