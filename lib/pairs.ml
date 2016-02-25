module Make(T1: ExtMap.FmtOrderedType)(T2: ExtMap.FmtOrderedType) = struct
  type t = T1.t * T2.t [@@deriving ord, show]
end
