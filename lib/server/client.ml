open Lwt.Syntax
open Mooncaml_shared

let src = Logs.Src.create "mooncaml_server.client" ~doc:"Interaction layer for connected clients"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)

type t =
  { id : int
  ; broadcast : Packet.t -> int -> unit Lwt.t
  ; pass_to_other_client : int -> Packet.t -> unit Lwt.t
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  }

let handle_says_me (packet : Packet.t) sender_id client =
  match packet with
  | Packet.SaysMe msg ->
    if sender_id = client.id
    then
      (* It came from our own connection so we want to broadcast it to others *)
      let* () = client.broadcast packet client.id in
      let response = Packet.SaysMeResponse (true, "Message broadcasted successfully") in
      Packet.send client.oc response
    else (
      (* It came from another client, so pass it on to our connection *)
      let says_other_packet = Packet.SaysOther (sender_id, msg) in
      Packet.send client.oc says_other_packet)
  | _ -> raise (Invalid_argument "Received non-say packet in handle_says_me")
;;

let handle_move_me (packet : Packet.t) sender_id client =
  match packet with
  | Packet.MoveMe (x, y) ->
    if sender_id = client.id
    then (
      (* It came from our own connection so we want to broadcast it to others *)
      let success = 0 <= x && x < 10 && 0 <= y && y < 10 in
      let* () = if success then client.broadcast packet client.id else Lwt.return_unit in
      let msg = if success then "" else "You can't go there!" in
      let response = Packet.MoveMeResponse (success, msg) in
      Packet.send client.oc response)
    else (
      (* It came from another client, so pass it on to our connection *)
      let move_other_packet = Packet.MoveOther (sender_id, x, y) in
      Packet.send client.oc move_other_packet)
  | _ -> raise (Invalid_argument "Received non-move packet in handle_move_me")
;;

let handle_connect_me (packet : Packet.t) sender_id client =
  match packet with
  | Packet.ConnectMe ->
    if sender_id = client.id
    then
      (* It came from our own connection so we want to broadcast it to others *)
      let* () = client.broadcast packet client.id in
      let response = Packet.ConnectMeResponse (true, "Connected successfully") in
      Packet.send client.oc response
    else (
      (* It came from another client, so pass it on to our connection and tell the new guy about us *)
      let connect_other_packet = Packet.ConnectOther sender_id in
      let* () = Packet.send client.oc connect_other_packet in
      let my_player_info = Packet.{ name = Printf.sprintf "Player%d" client.id; x = 0; y = 0 } in
      let about_me_packet = Packet.AboutOther (client.id, my_player_info) in
      client.pass_to_other_client sender_id about_me_packet)
  | _ -> raise (Invalid_argument "Received non-connect packet in handle_connect_me")
;;

let handle_about_other (packet : Packet.t) sender_id client =
  match packet with
  | Packet.AboutOther (other_client_id, _) ->
    if sender_id <> other_client_id
    then raise (Invalid_argument "Sender ID does not match player info ID in handle_about_other")
    else if sender_id = client.id
    then
      raise
        (Invalid_argument "Received about_other packet from our own client in handle_about_other")
    else
      (* It came from another client, so pass it on to our connection *)
      Packet.send client.oc packet
  | _ -> raise (Invalid_argument "Received non-about_other packet in handle_about_other")
;;

let handle_disconnect_me (packet : Packet.t) sender_id client =
  match packet with
  | Packet.DisconnectMe ->
    if sender_id = client.id
    then
      (* It came from our own connection so we want to broadcast it to others *)
      client.broadcast packet client.id
    else (
      (* It came from another client, so pass it on to our connection *)
      let disconnect_other_packet = Packet.DisconnectOther sender_id in
      Packet.send client.oc disconnect_other_packet)
  | _ -> raise (Invalid_argument "Received non-disconnect packet in handle_disconnect_me")
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
  | Packet.SaysMe _ -> handle_says_me packet sender_id client
  | Packet.MoveMe _ -> handle_move_me packet sender_id client
  | Packet.ConnectMe -> handle_connect_me packet sender_id client
  | Packet.DisconnectMe -> handle_disconnect_me packet sender_id client
  (* Passed on from another server client *)
  | Packet.AboutOther _ -> handle_about_other packet sender_id client
  | _ ->
    Log_lwt.warn (fun m ->
      m "Received unrecognized packet from client %d: %s" sender_id (Packet.string_of_packet packet))
;;
