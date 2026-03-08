open Sexplib.Std

let src = Logs.Src.create "mooncaml_shared.packet" ~doc:"Network packets"

module Log = (val Logs.src_log src : Logs.LOG)

type player_info =
  { name : string
  ; x : int
  ; y : int
  }
[@@deriving sexp]

type response = bool * string [@@deriving sexp]

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
  | AboutOther of int * player_info
  | DisconnectOther of int
  (* From the server directly *)
  | UnexpectedServerError of string
  | SaysMeResponse of response
  | MoveMeResponse of response
  | ConnectMeResponse of response
  | DisconnectMeResponse of response
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
