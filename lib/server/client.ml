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
  ; player : Entities.player
  ; get_other_players : int -> Entities.player list
  }

let handle_chat_command (packet : Packet.t) sender_id client =
  match packet with
  | Packet.ChatCommand msg ->
    if sender_id = client.id
    then
      (* It came from our own connection so we want to broadcast it to others *)
      let* () =
        client.broadcast (Packet.ChatEvent { sender_id = client.id; message = msg }) client.id
      in
      let response = Packet.ChatCommandResponse (true, "Message broadcasted successfully") in
      Packet.send client.oc response
    else (
      (* It came from another client, so pass it on to our connection *)
      let event = Packet.ChatEvent { sender_id; message = msg } in
      Packet.send client.oc event)
  | _ -> raise (Invalid_argument "Received non-chat packet in handle_chat_command")
;;

let handle_move_command (packet : Packet.t) sender_id client =
  match packet with
  | Packet.MoveCommand { x; y } ->
    if sender_id = client.id
    then (
      (* It came from our own connection so we want to broadcast it to others *)
      let success = 0 <= x && x < 10 && 0 <= y && y < 10 in
      let* () =
        if success
        then client.broadcast (Packet.MoveEvent { sender_id = client.id; x; y }) client.id
        else Lwt.return_unit
      in
      let msg = if success then "" else "You can't go there!" in
      let response = Packet.MoveCommandResponse (success, msg) in
      Packet.send client.oc response)
    else (
      (* It came from another client, so pass it on to our connection *)
      let event = Packet.MoveEvent { sender_id; x; y } in
      Packet.send client.oc event)
  | _ -> raise (Invalid_argument "Received non-move packet in handle_move_command")
;;

let handle_connect_command sender_id client =
  if sender_id = client.id
  then (
    (* Send WelcomeEvent with our player info and all other connected players *)
    let other_players = client.get_other_players client.id in
    let welcome = Packet.WelcomeEvent { your_player = client.player; other_players } in
    let* () = Packet.send client.oc welcome in
    (* Broadcast our existence to others *)
    client.broadcast (Packet.PlayerInfoEvent client.player) client.id)
  else (
    (* It came from another client — they just connected, send them our player info *)
    let event = Packet.PlayerInfoEvent client.player in
    Packet.send client.oc event)
;;

let handle_disconnect_command (packet : Packet.t) sender_id client =
  match packet with
  | Packet.DisconnectCommand ->
    if sender_id = client.id
    then
      (* It came from our own connection so we want to broadcast it to others *)
      client.broadcast (Packet.DisconnectEvent { sender_id = client.id }) client.id
    else (
      (* It came from another client, so pass it on to our connection *)
      let event = Packet.DisconnectEvent { sender_id } in
      Packet.send client.oc event)
  | _ -> raise (Invalid_argument "Received non-disconnect packet in handle_disconnect_command")
;;

let handle_move_event (packet : Packet.t) sender_id client =
  match packet with
  | Packet.MoveEvent { sender_id = packet_sid; x; y } ->
    if sender_id = client.id
    then
      Log_lwt.warn (fun m ->
        m "Received MoveEvent from client %d for itself, which should not happen" sender_id)
    else if sender_id != packet_sid
    then
      Log_lwt.warn (fun m ->
        m "Received MoveEvent from client %d with mismatched sender_id %d" sender_id packet_sid)
    else (
      (* Coming from another client, we'll want to pass this on to our connection *)
      let event = Packet.MoveEvent { sender_id; x; y } in
      Packet.send client.oc event)
  | _ -> raise (Invalid_argument "Received non-move event packet in handle_move_event")
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
  (* Directly from the connected user *)
  | Packet.ChatCommand _ -> handle_chat_command packet sender_id client
  | Packet.MoveCommand _ -> handle_move_command packet sender_id client
  | Packet.ConnectCommand -> handle_connect_command sender_id client
  | Packet.DisconnectCommand -> handle_disconnect_command packet sender_id client
  | Packet.MoveEvent _ -> handle_move_event packet sender_id client
  | _ ->
    Log_lwt.warn (fun m ->
      m "Received unrecognized packet from client %d: %s" sender_id (Packet.string_of_packet packet))
;;
