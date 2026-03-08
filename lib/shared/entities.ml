open Sexplib.Std

type player =
  { id : int
  ; name : string
  ; x : int
  ; y : int
  }
[@@deriving sexp]
