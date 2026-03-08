open Lwt.Syntax
open Mooncaml_shared

let src = Logs.Src.create "mooncaml_server.client" ~doc:"Interaction layer for connected clients"

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
  | Packet.Say _ ->
    if sender_id = client.id
    then
      (* It came from our own connection so we want to broadcast it to others *)
      let* () = client.broadcast packet client.id in
      let response = Packet.SayResponse (true, "Message broadcasted successfully") in
      Lwt_io.write_line client.oc (Packet.string_of_packet response)
    else
      (* It came from another client, so pass it on to our connection *)
      Lwt_io.write_line client.oc (Packet.string_of_packet packet)
  | _ -> raise (Invalid_argument "Received non-say packet in handle_say")
;;

let handle_move (packet : Packet.t) sender_id client =
  match packet with
  | Packet.Move (x, y) ->
    if sender_id = client.id
    then (
      (* It came from our own connection so we want to broadcast it to others *)
      let success = 0 <= x && x < 10 && 0 <= y && y < 10 in
      let* () = if success then client.broadcast packet client.id else Lwt.return_unit in
      let msg = if success then "" else "You can't go there!" in
      let response = Packet.MoveResponse (success, msg) in
      Lwt_io.write_line client.oc (Packet.string_of_packet response))
    else
      (* It came from another client, so pass it on to our connection *)
      Lwt_io.write_line client.oc (Packet.string_of_packet packet)
  | _ -> raise (Invalid_argument "Received non-move packet in handle_move")
;;

let handle_disconnect (packet : Packet.t) sender_id client =
  match packet with
  | Packet.Disconnect ->
    if sender_id = client.id
    then client.broadcast packet client.id
    else Lwt_io.write_line client.oc (Printf.sprintf "Client %d has disconnected" sender_id)
  | _ -> raise (Invalid_argument "Received non-disconnect packet in handle_disconnect")
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
  | Packet.Disconnect -> handle_disconnect packet sender_id client
  | _ ->
    Log_lwt.warn (fun m ->
      m "Received unrecognized packet from client %d: %s" sender_id (Packet.string_of_packet packet))
;;
