open Sexplib.Std

type t =
  | Say of string
  | Move of int * int
[@@deriving sexp]

let packet_of_string s =
  try Ok (t_of_sexp (Sexplib.Sexp.of_string s)) with
  | exn -> Error (Printexc.to_string exn)
;;

let string_of_packet p = Sexplib.Sexp.to_string (sexp_of_t p)
