open Lwt.Syntax
open Mooncaml_shared

let src = Logs.Src.create "mooncaml_server.client" ~doc:"Interaction layer for connected clients"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)
module Db = Mooncaml_server_db

type session_state =
  | Lobby
  | InGame of
      { player : Entities.player
      ; user_id : int
      }

type t =
  { id : int
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  ; db_pool : Db.Types.pool
  ; map : Maps.map_data
  ; broadcast : Packet.t -> int -> unit Lwt.t
  ; try_move_player : int -> int -> bool
  ; get_all_players : unit -> Entities.player list
  ; spawn_player : string -> Entities.player
  ; remove_player_from_world : int -> unit
  }

let gen_salt len =
  let rand_char _ = Char.chr (65 + Random.int 26) in
  String.init len rand_char
;;

let hash_password pwd =
  let t_cost = 2 in
  let m_cost = 65536 in
  let parallelism = 1 in
  let hash_len = 32 in
  let salt_len = 16 in
  let salt = gen_salt salt_len in
  let encoded_len =
    Argon2.encoded_len ~t_cost ~m_cost ~parallelism ~salt_len ~hash_len ~kind:Argon2.ID
  in
  match Argon2.ID.hash_encoded ~t_cost ~m_cost ~parallelism ~hash_len ~encoded_len ~pwd ~salt with
  | Result.Ok encoded -> Argon2.ID.encoded_to_string encoded
  | Result.Error e -> failwith (Argon2.ErrorCodes.message e)
;;

let verify_password ~encoded ~pwd =
  match Argon2.verify ~encoded ~pwd ~kind:Argon2.ID with
  | Result.Ok true -> true
  | Result.Ok false -> false
  | Result.Error Argon2.ErrorCodes.VERIFY_MISMATCH -> false
  | Result.Error e ->
    Logs.err (fun m -> m "Argon2 verify error: %s" (Argon2.ErrorCodes.message e));
    false
;;

module StateInLobby = struct
  let handle_login_command client username password =
    let* user_opt = Db.Queries.get_user_opt_by_username client.db_pool username in
    match user_opt with
    | Some { id = user_id; password_hash = hash; _ } ->
      if verify_password ~encoded:hash ~pwd:password
      then
        let* () = Log_lwt.info (fun m -> m "User %s authenticated successfully" username) in
        let player = client.spawn_player username in
        let other_players =
          client.get_all_players () |> List.filter (fun (p : Entities.player) -> p.id <> client.id)
        in
        let welcome =
          Packet.WelcomeEvent { your_player = player; other_players; map_name = client.map.name }
        in
        let* () = Packet.send client.oc welcome in
        let* () = client.broadcast (Packet.PlayerInfoEvent player) client.id in
        Lwt.return (InGame { player; user_id })
      else
        let* () =
          Log_lwt.warn (fun m -> m "Failed login attempt for user %s: incorrect password" username)
        in
        let* () =
          Packet.send
            client.oc
            (Packet.LoginCommandResponse (false, "Invalid username or password"))
        in
        Lwt.return Lobby
    | None ->
      let* () = Packet.send client.oc (Packet.LoginCommandResponse (false, "User not found")) in
      Lwt.return Lobby
  ;;

  let handle_register_command client username password =
    let* user_opt = Db.Queries.get_user_opt_by_username client.db_pool username in
    match user_opt with
    | Some _ ->
      let* () =
        Packet.send client.oc (Packet.RegisterCommandResponse (false, "Username already taken"))
      in
      Lwt.return Lobby
    | None ->
      let hash_res =
        try
          let hash = hash_password password in
          Ok ((), hash)
        with
        | e -> Error (Printexc.to_string e)
      in
      (match hash_res with
       | Ok (_, hash) ->
         let* insert_result = Db.Queries.create_user client.db_pool username hash in
         (match insert_result with
          | Ok (Ok ()) ->
            let* () = Log_lwt.info (fun m -> m "User %s registered successfully" username) in
            let* () =
              Packet.send
                client.oc
                (Packet.RegisterCommandResponse
                   (true, "Registration successful! You may now log in."))
            in
            Lwt.return Lobby
          | _ ->
            let* () = Packet.send client.oc (Packet.UnexpectedServerError "Database error") in
            Lwt.return Lobby)
       | Error _ ->
         let* () = Packet.send client.oc (Packet.UnexpectedServerError "Crypto error") in
         Lwt.return Lobby)
  ;;

  let handle_packet client packet =
    match packet with
    | Packet.LoginCommand { username; password } -> handle_login_command client username password
    | Packet.RegisterCommand { username; password } ->
      handle_register_command client username password
    | Packet.DisconnectCommand -> Lwt.return Lobby
    | _ ->
      let* () = Log_lwt.warn (fun m -> m "Client %d sent game packet while in Lobby" client.id) in
      Lwt.return Lobby
  ;;
end

module StateInGame = struct
  let handle_chat_command client (player : Entities.player) user_id (packet : Packet.t) =
    match packet with
    | Packet.ChatCommand msg ->
      let* () =
        client.broadcast (Packet.ChatEvent { sender_id = client.id; message = msg }) client.id
      in
      let* () =
        Packet.send
          client.oc
          (Packet.ChatCommandResponse (true, "Message broadcasted successfully"))
      in
      Lwt.return (InGame { player; user_id })
    | _ -> raise (Invalid_argument "Received non-chat packet in handle_chat_command")
  ;;

  let handle_move_command client (player : Entities.player) user_id (packet : Packet.t) =
    match packet with
    | Packet.MoveCommand { x; y } ->
      let success = client.try_move_player x y in
      let* () =
        if success
        then client.broadcast (Packet.MoveEvent { sender_id = client.id; x; y }) client.id
        else Lwt.return_unit
      in
      let actual_player = if success then Entities.{ player with x; y } else player in
      let msg = if success then "" else "You bumped into a wall!" in
      let response =
        Packet.MoveCommandResponse { success; msg; x = actual_player.x; y = actual_player.y }
      in
      let* () = Packet.send client.oc response in
      Lwt.return (InGame { player = actual_player; user_id })
    | _ -> raise (Invalid_argument "Received non-move packet in handle_move_command")
  ;;

  let handle_logout_command client =
    let* () = Log_lwt.info (fun m -> m "User %d logged out" client.id) in
    client.remove_player_from_world client.id;
    let* () = client.broadcast (Packet.DisconnectEvent { sender_id = client.id }) client.id in
    Lwt.return Lobby
  ;;

  let handle_packet client (player : Entities.player) user_id packet =
    match packet with
    | Packet.ChatCommand _ -> handle_chat_command client player user_id packet
    | Packet.MoveCommand _ -> handle_move_command client player user_id packet
    | Packet.DisconnectCommand | Packet.LogoutCommand -> handle_logout_command client
    | _ ->
      let* () = Log_lwt.warn (fun m -> m "Received unrecognized packet from client %d" client.id) in
      Lwt.return (InGame { player; user_id })
  ;;
end

let rec loop client state =
  let* line_opt = Lwt_io.read_line_opt client.ic in
  match line_opt with
  | None -> Lwt.return_unit
  | Some line ->
    let* next_state =
      match Packet.packet_of_string line with
      | Ok packet ->
        (match state with
         | Lobby -> StateInLobby.handle_packet client packet
         | InGame { player; user_id } -> StateInGame.handle_packet client player user_id packet)
      | Error err ->
        let* () = Log_lwt.err (fun m -> m "Parse error from client %d: %s" client.id err) in
        let* () = Packet.send client.oc (Packet.UnexpectedServerError "Failed to parse packet") in
        Lwt.return state
    in
    loop client next_state
;;

let start client =
  Lwt.finalize
    (fun () -> loop client Lobby)
    (fun () ->
       let* () =
         Log_lwt.info (fun m -> m "Disconnected" ~tags:(Logging.tag_with_client client.id))
       in
       client.remove_player_from_world client.id;
       let* () = client.broadcast (Packet.DisconnectEvent { sender_id = client.id }) client.id in
       let* () = Lwt_io.close client.ic in
       Lwt_io.close client.oc)
;;
