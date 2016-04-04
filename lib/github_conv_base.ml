open Sexplib.Std

module Int64 = struct
  type t = int64 [@@deriving sexp]
end
