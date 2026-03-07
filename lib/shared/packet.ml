open Sexplib.Std

let src = Logs.Src.create "mooncaml_shared.packet" ~doc:"Network packets"

module Log = (val Logs.src_log src : Logs.LOG)

type t =
  | Say of string
  | SayResponse of (bool * string)
  | Move of int * int
  | MoveResponse of (bool * string)
  | Disconnect
[@@deriving sexp]

let packet_of_string s =
  try Ok (t_of_sexp (Sexplib.Sexp.of_string s)) with
  | exn -> Error (Printexc.to_string exn)
;;

let string_of_packet p = Sexplib.Sexp.to_string (sexp_of_t p)
