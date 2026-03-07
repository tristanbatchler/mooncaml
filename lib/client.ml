open Lwt.Syntax

let src = Logs.Src.create "mooncaml.client" ~doc:"Interaction layer for connected clients"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)

type t =
  { id : int
  ; broadcast : Packet.t -> int -> unit Lwt.t
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  }

let handle_say (packet : Packet.t) sender_id client =
  match packet with
  | Packet.Say msg ->
    if sender_id = client.id
    then client.broadcast packet client.id
    else Lwt_io.write_line client.oc (Printf.sprintf "Client %d says: %s" sender_id msg)
  | _ -> raise (Invalid_argument "Received non-say packet in handle_say")
;;

let handle_move (packet : Packet.t) sender_id client =
  match packet with
  | Packet.Move (x, y) ->
    if sender_id = client.id
    then client.broadcast packet client.id
    else Lwt_io.write_line client.oc (Printf.sprintf "Client %d moved to (%d, %d)" sender_id x y)
  | _ -> raise (Invalid_argument "Received non-move packet in handle_move")
;;

let handle_packet packet sender_id client =
  let* () =
    Log_lwt.debug (fun m ->
      m
        "Handling packet from client %d: %s"
        sender_id
        (Packet.string_of_packet packet)
        ~tags:(Logging.tag_with_client client.id))
  in
  match packet with
  | Packet.Say _ -> handle_say packet sender_id client
  | Packet.Move _ -> handle_move packet sender_id client
;;
