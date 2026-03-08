open Lwt.Syntax
open Mooncaml_shared

let src = Logs.Src.create "mooncaml_server.client" ~doc:"Interaction layer for connected clients"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)

type t =
  { id : int
  ; broadcast : Packet.t -> int -> unit Lwt.t
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel (* Server Logic Hooks *)
  ; try_move : int -> int -> bool
  ; get_all_players : unit -> Entities.player list
  ; get_player : unit -> Entities.player
  ; map : Maps.map_data
  }

let handle_chat_command (packet : Packet.t) client =
  match packet with
  | Packet.ChatCommand msg ->
    let* () =
      client.broadcast (Packet.ChatEvent { sender_id = client.id; message = msg }) client.id
    in
    Packet.send client.oc (Packet.ChatCommandResponse (true, "Message broadcasted successfully"))
  | _ -> raise (Invalid_argument "Received non-chat packet in handle_chat_command")
;;

let handle_move_command (packet : Packet.t) client =
  match packet with
  | Packet.MoveCommand { x; y } ->
    (* Ask the Hub to apply the physics *)
    let success = client.try_move x y in
    let* () =
      if success
      then client.broadcast (Packet.MoveEvent { sender_id = client.id; x; y }) client.id
      else Lwt.return_unit
    in
    (* Fetch the ABSOLUTE TRUTH from the Hub *)
    let actual_player = client.get_player () in
    let msg = if success then "" else "You bumped into a wall!" in
    let response =
      Packet.MoveCommandResponse { success; msg; x = actual_player.x; y = actual_player.y }
    in
    Packet.send client.oc response
  | _ -> raise (Invalid_argument "Received non-move packet in handle_move_command")
;;

let handle_connect_command client =
  (* Grab the full world state from the Hub *)
  let all_players = client.get_all_players () in
  let your_player, other_players =
    List.partition (fun (p : Entities.player) -> p.id = client.id) all_players
  in
  let your_player =
    match your_player with
    | [ p ] -> p
    | _ -> failwith "Could not find own player in world state"
  in
  let welcome = Packet.WelcomeEvent { your_player; other_players; map_name = client.map.name } in
  let* () = Packet.send client.oc welcome in
  (* Broadcast our existence to others *)
  client.broadcast (Packet.PlayerInfoEvent your_player) client.id
;;

let handle_disconnect_command (packet : Packet.t) client =
  match packet with
  | Packet.DisconnectCommand ->
    client.broadcast (Packet.DisconnectEvent { sender_id = client.id }) client.id
  | _ -> raise (Invalid_argument "Received non-disconnect packet in handle_disconnect_command")
;;

let handle_packet packet _sender_id client =
  let* () =
    Log_lwt.debug (fun m ->
      m
        "Handling packet from client %d: %s"
        client.id
        (Packet.string_of_packet packet)
        ~tags:(Logging.tag_with_client client.id))
  in
  match packet with
  | Packet.ChatCommand _ -> handle_chat_command packet client
  | Packet.MoveCommand _ -> handle_move_command packet client
  | Packet.ConnectCommand -> handle_connect_command client
  | Packet.DisconnectCommand -> handle_disconnect_command packet client
  | _ ->
    Log_lwt.warn (fun m ->
      m "Received unrecognized packet from client %d: %s" client.id (Packet.string_of_packet packet))
;;
