open Sexplib.Std

let src = Logs.Src.create "mooncaml_shared.packet" ~doc:"Network packets"

module Log = (val Logs.src_log src : Logs.LOG)

type response = bool * string [@@deriving sexp]

type t =
  (* From the user *)
  | ChatCommand of string
  | MoveCommand of
      { x : int
      ; y : int
      }
  | ConnectCommand
  | DisconnectCommand
  (* From another client (forwarded by the server) *)
  | ChatEvent of
      { sender_id : int
      ; message : string
      }
  | MoveEvent of
      { sender_id : int
      ; x : int
      ; y : int
      }
  | DisconnectEvent of { sender_id : int }
  (* From the server directly *)
  | UnexpectedServerError of string
  | ChatCommandResponse of response
  | MoveCommandResponse of
      { success : bool
      ; msg : string
      ; x : int
      ; y : int
      }
  | ConnectCommandResponse of response
  | DisconnectCommandResponse of response
  | PlayerInfoEvent of Entities.player
  | WelcomeEvent of
      { your_player : Entities.player
      ; other_players : Entities.player list
      }
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
