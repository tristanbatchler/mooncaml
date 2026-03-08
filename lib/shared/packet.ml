open Sexplib.Std

let src = Logs.Src.create "mooncaml_shared.packet" ~doc:"Network packets"

module Log = (val Logs.src_log src : Logs.LOG)

type t =
  (* From the user *)
  | SaysMe of string
  | MoveMe of int * int
  | ConnectMe
  | DisconnectMe
  (* From another client (forwarded by the server) *)
  | SaysOther of int * string
  | MoveOther of int * int * int
  | ConnectOther of int
  | DisconnectOther of int
  (* From the server directly *)
  | UnexpectedServerError of string
  | SaysMeResponse of (bool * string)
  | MoveMeResponse of (bool * string)
  | ConnectMeResponse of (bool * string)
  | DisconnectMeResponse of (bool * string)
[@@deriving sexp]

let packet_of_string s =
  try Ok (t_of_sexp (Sexplib.Sexp.of_string s)) with
  | exn -> Error (Printexc.to_string exn)
;;

let string_of_packet p = Sexplib.Sexp.to_string (sexp_of_t p)

let send oc packet =
  let s = string_of_packet packet in
  Log.debug (fun m -> m "Sending packet: %s" s);
  Lwt_io.write_line oc s
;;
