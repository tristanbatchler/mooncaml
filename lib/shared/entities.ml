open Sexplib.Std

type player =
  { name : string
  ; x : int
  ; y : int
  }
[@@deriving sexp]
