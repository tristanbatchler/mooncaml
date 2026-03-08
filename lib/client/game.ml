open Lwt.Syntax
open Mooncaml_shared

let server_instream, server_outstream = Lwt_stream.create ()

let handle_chat_event sender_id message state =
  state |> Input.add_logf "Client %d says: %s" sender_id message
;;

let handle_move_event sender_id x y (state : Types.state) =
  match Types.IntMap.find_opt sender_id state.other_players with
  | Some other ->
    let updated_other = { other with x; y } in
    let updated_others = Types.IntMap.add sender_id updated_other state.other_players in
    { state with other_players = updated_others }
  | None ->
    Logs.warn (fun m -> m "Received MoveEvent for unknown sender_id %d" sender_id);
    state
;;

let handle_connect_event sender_id state =
  state |> Input.add_logf "Client %d has connected" sender_id
;;

let handle_player_info (player_info : Entities.player) (state : Types.state) =
  { state with other_players = Types.IntMap.add player_info.id player_info state.other_players }
  |> Input.add_logf "%s appeared at (%d, %d)" player_info.name player_info.x player_info.y
;;

let handle_disconnect_event sender_id state =
  state |> Input.add_logf "Client %d has disconnected" sender_id
;;

(* ----- From the server directly -------------------------- *)

let handle_unexpected_server_error msg state = state |> Input.add_log ("Server error: " ^ msg)

let handle_chat_command_response success msg state =
  if success then state else state |> Input.add_log ("Failed to send message: " ^ msg)
;;

let handle_move_command_response success msg (state : Types.state) =
  (* There are issues with this - one being moving feels ever so slighly delayed, 
     another being that this response might be for a previous move command, which 
     would snap the player back to a previous location unexpectedly *)
  if success
  then (
    let x, y = state.desired_location in
    { state with player = { state.player with x; y } })
  else state |> Input.add_log ("Failed to move: " ^ msg)
;;

let handle_disconnect_command_response success msg state =
  if success then state else state |> Input.add_log ("Failed to disconnect: " ^ msg)
;;

let handle_packet (state : Types.state) packet =
  match packet with
  (* From another client (forwarded by the server) *)
  | Packet.ChatEvent { sender_id; message } -> state |> handle_chat_event sender_id message
  | Packet.MoveEvent { sender_id; x; y } -> state |> handle_move_event sender_id x y
  | Packet.ConnectEvent { sender_id } -> state |> handle_connect_event sender_id
  | Packet.PlayerInfoEvent player_info -> state |> handle_player_info player_info
  | Packet.DisconnectEvent { sender_id } -> state |> handle_disconnect_event sender_id
  (* From the server directly *)
  | Packet.UnexpectedServerError msg -> state |> handle_unexpected_server_error msg
  | Packet.ChatCommandResponse (success, msg) -> state |> handle_chat_command_response success msg
  | Packet.MoveCommandResponse (success, msg) -> state |> handle_move_command_response success msg
  | Packet.DisconnectCommandResponse (success, msg) ->
    state |> handle_disconnect_command_response success msg
  | _ ->
    Logs.warn (fun m -> m "Received unexpected packet: %s" (Packet.string_of_packet packet));
    state
;;

let rec send_out_packets oc = function
  | [] -> Lwt.return_unit
  | packet :: rest ->
    let* () = Packet.send oc packet in
    send_out_packets oc rest
;;

let rec game_loop state ic oc =
  let incoming = Lwt_stream.get_available server_instream in
  let state = Windows.handle_resize @@ List.fold_left handle_packet state incoming in
  let* () = send_out_packets oc (List.rev state.send_packets) in
  let state = { state with send_packets = [] } in
  Drawing.draw_map state;
  Drawing.draw_log state;
  Drawing.draw_chat state;
  let ch = Curses.getch () in
  let next_state =
    if ch <> -1 && ch <> Curses.Key.resize
    then (
      match state.mode with
      | Chat -> Input.handle_chat_input state ch
      | World -> Input.handle_game_input state ch)
    else state
  in
  let* () = Lwt.pause () in
  game_loop next_state ic oc
;;

let run ic oc () =
  (* Make ESC key work immediately *)
  Unix.putenv "ESCDELAY" "25";
  let _stdscr = Curses.initscr () in
  at_exit Curses.endwin;
  ignore (Curses.cbreak ());
  ignore (Curses.noecho ());
  ignore (Curses.keypad (Curses.stdscr ()) true);
  ignore (Curses.curs_set 0);
  Curses.winch_handler_on ();
  Curses.timeout 50;
  let* () = Packet.send oc Packet.ConnectCommand in
  (* Wait for the server to respond with a WelcomeEvent containing our player and existing players *)
  let rec wait_for_welcome () =
    let* packet = Lwt_stream.get server_instream in
    match packet with
    | Some (Packet.WelcomeEvent { your_player; other_players }) ->
      Lwt.return (your_player, other_players)
    | _ -> wait_for_welcome ()
  in
  let* player, other_players = wait_for_welcome () in
  let others_map =
    List.fold_left
      (fun m (p : Entities.player) -> Types.IntMap.add p.id p m)
      Types.IntMap.empty
      other_players
  in
  let initial_state =
    { ui = Windows.create_windows ()
    ; log = []
    ; chat = Textbox.empty_edit
    ; player
    ; mode = Types.World
    ; send_packets = []
    ; other_players = others_map
    ; desired_location = player.x, player.y
    }
    |> Input.add_logf "Welcome, %s!" player.name
  in
  game_loop initial_state ic oc
;;
