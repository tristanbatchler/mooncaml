open Sexplib.Std

type player =
  { client_id : int
  ; name : string
  ; x : int
  ; y : int
  }
[@@deriving sexp]
