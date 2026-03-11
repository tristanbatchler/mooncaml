open Lwt.Syntax
open Mooncaml_shared

let server_instream, server_outstream = Lwt_stream.create ()

let handle_chat_event sender_id message state =
  state |> Input.add_logf "Client %d says: %s" sender_id message
;;

let handle_move_event sender_id x y (state : Types.game_state) =
  match Types.IntMap.find_opt sender_id state.other_players with
  | Some other ->
    let updated_other = { other with x; y } in
    let updated_others = Types.IntMap.add sender_id updated_other state.other_players in
    { state with other_players = updated_others }
  | None ->
    Logs.warn (fun m -> m "Received MoveEvent for unknown sender_id %d" sender_id);
    state
;;

let handle_player_info (player_info : Entities.player) (state : Types.game_state) =
  { state with other_players = Types.IntMap.add player_info.id player_info state.other_players }
  |> Input.add_logf "%s appeared at (%d, %d)" player_info.name player_info.x player_info.y
;;

let handle_disconnect_event sender_id (state : Types.game_state) =
  { state with other_players = Types.IntMap.remove sender_id state.other_players }
  |> Input.add_logf "Client %d has disconnected" sender_id
;;

(* ----- From the server directly -------------------------- *)

let handle_unexpected_server_error msg state = state |> Input.add_log ("Server error: " ^ msg)

let handle_chat_command_response success msg state =
  if success then state else state |> Input.add_log ("Failed to send message: " ^ msg)
;;

let handle_move_command_response success _ x y (state : Types.game_state) =
  if success then state else { state with player = { state.player with x; y } }
;;

let handle_disconnect_command_response success msg state =
  if success then state else state |> Input.add_log ("Failed to disconnect: " ^ msg)
;;

let handle_packet (state : Types.client_state) packet =
  match state.mode, packet with
  (* If we receive a WelcomeEvent while on the Title screen, initialize the Game UI! *)
  | Types.Title _, Packet.WelcomeEvent { your_player; other_players; map_name } ->
    let map =
      match Maps.map_opt_of_string map_name with
      | Some m -> m
      | None -> failwith "Unknown map"
    in
    let others_map =
      List.fold_left
        (fun m (p : Entities.player) -> Types.IntMap.add p.id p m)
        Types.IntMap.empty
        other_players
    in
    let g_state : Types.game_state =
      { ui = Windows.create_windows ()
      ; log = [ "Welcome to Mooncaml!" ]
      ; chat = Textbox.empty_edit
      ; player = your_player
      ; focus = MapWindow
      ; log_scroll_offset = 0
      ; other_players = others_map
      ; map
      ; popup = NoPopup
      }
    in
    { state with mode = Game g_state }
  (* --- Auth Responses on the Title Screen --- *)
  | Types.Title _, Packet.LoginCommandResponse (false, msg) ->
    let popup = Types.MessageBox { title = "Login Failed"; message = msg } in
    { state with mode = Title { popup } }
  | Types.Title _, Packet.RegisterCommandResponse (success, msg) ->
    let title = if success then "Success" else "Registration Failed" in
    let popup = Types.MessageBox { title; message = msg } in
    { state with mode = Title { popup } }
  | Types.Title _, Packet.UnexpectedServerError msg ->
    let popup = Types.MessageBox { title = "Server Error"; message = msg } in
    { state with mode = Title { popup } }
  (* --- In-game packets --- *)
  | Types.Game g_state, _ ->
    let new_g_state =
      match packet with
      (* From another client (forwarded by the server) *)
      | Packet.ChatEvent { sender_id; message } -> g_state |> handle_chat_event sender_id message
      | Packet.MoveEvent { sender_id; x; y } -> g_state |> handle_move_event sender_id x y
      | Packet.PlayerInfoEvent player_info -> g_state |> handle_player_info player_info
      | Packet.DisconnectEvent { sender_id } -> g_state |> handle_disconnect_event sender_id
      (* From the server directly *)
      | Packet.UnexpectedServerError msg -> g_state |> handle_unexpected_server_error msg
      | Packet.ChatCommandResponse (success, msg) ->
        g_state |> handle_chat_command_response success msg
      | Packet.MoveCommandResponse { success; msg; x; y } ->
        g_state |> handle_move_command_response success msg x y
      | Packet.DisconnectCommandResponse (success, msg) ->
        g_state |> handle_disconnect_command_response success msg
      | _ ->
        Logs.warn (fun m -> m "Received unexpected packet: %s" (Packet.string_of_packet packet));
        g_state
    in
    { state with mode = Game new_g_state }
  | _ -> state
;;

let rec send_out_packets oc = function
  | [] -> Lwt.return_unit
  | packet :: rest ->
    let* () = Packet.send oc packet in
    send_out_packets oc rest
;;

let rec app_loop (state : Types.client_state) ic oc =
  let incoming = Lwt_stream.get_available server_instream in
  let state = List.fold_left handle_packet state incoming in
  (* Only handle resize if we are in the game *)
  let state =
    match state.mode with
    | Game g -> { state with mode = Game { g with ui = Windows.handle_resize g.ui } }
    | Title _ -> state
  in
  (* Send packets and IMMEDIATELY generate a clean state *)
  let* () = send_out_packets oc (List.rev state.send_packets) in
  let clean_state = { state with send_packets = [] } in
  Drawing.draw_app clean_state;
  let ch = Curses.getch () in
  let next_state =
    if ch <> -1 && ch <> Curses.Key.resize
    then Input.handle_input clean_state ch (* Pass the clean state! *)
    else clean_state
  in
  let* () = Lwt.pause () in
  app_loop next_state ic oc
;;

let run ic oc () =
  Unix.putenv "ESCDELAY" "25";
  let _stdscr = Curses.initscr () in
  at_exit Curses.endwin;
  ignore (Curses.cbreak ());
  ignore (Curses.noecho ());
  ignore (Curses.keypad (Curses.stdscr ()) true);
  Curses.winch_handler_on ();
  Curses.timeout 50;
  let initial_state : Types.client_state =
    { mode =
        Title
          { popup =
              ChoiceMenu
                { title = "Mooncaml"
                ; options = [ "Login"; "Register"; "Quit" ]
                ; selected = 0
                ; id = MenuTitle
                }
          }
    ; send_packets = []
    }
  in
  app_loop initial_state ic oc
;;
