open Lwt.Syntax
open Mooncaml_shared

let server_instream, server_outstream = Lwt_stream.create ()

let handle_says_other sender_id msg state =
  state |> Input.add_logf "Client %d says: %s" sender_id msg
;;

let handle_move_other sender_id x y state =
  state |> Input.add_logf "Client %d moves to (%d, %d)" sender_id x y
;;

let handle_connect_other sender_id state =
  state |> Input.add_logf "Client %d has connected" sender_id
;;

let handle_about_other sender_id (player_info : Entities.player) state =
  state
  |> Input.add_logf
       "Client %d is named %s and is at (%d, %d)"
       sender_id
       player_info.name
       player_info.x
       player_info.y
;;

let handle_disconnect_other sender_id state =
  state |> Input.add_logf "Client %d has disconnected" sender_id
;;

(* ----- From the server directly -------------------------- *)

let handle_unexpected_server_error msg state = state |> Input.add_log ("Server error: " ^ msg)

let handle_says_me_response success msg state =
  if success then state else state |> Input.add_log ("Failed to send message: " ^ msg)
;;

let handle_move_me_response success msg state =
  if success then state else state |> Input.add_log ("Failed to move: " ^ msg)
;;

let handle_connect_me_response success msg state =
  if success then state else state |> Input.add_log ("Failed to connect: " ^ msg)
;;

let handle_about_me (player : Entities.player) (state : Types.state) =
  { state with player } |> Input.add_logf "Welcome, %s!" player.name
;;

let handle_disconnect_me_response success msg state =
  if success then state else state |> Input.add_log ("Failed to disconnect: " ^ msg)
;;

let handle_packet (state : Types.state) packet =
  match packet with
  (* From another client (forwarded by the server) *)
  | Packet.SaysOther (sender_id, msg) -> state |> handle_says_other sender_id msg
  | Packet.MoveOther (sender_id, x, y) -> state |> handle_move_other sender_id x y
  | Packet.ConnectOther sender_id -> state |> handle_connect_other sender_id
  | Packet.AboutOther (sender_id, player_info) -> state |> handle_about_other sender_id player_info
  | Packet.DisconnectOther sender_id -> state |> handle_disconnect_other sender_id
  (* From the server directly *)
  | Packet.UnexpectedServerError msg -> state |> handle_unexpected_server_error msg
  | Packet.SaysMeResponse (success, msg) -> state |> handle_says_me_response success msg
  | Packet.MoveMeResponse (success, msg) -> state |> handle_move_me_response success msg
  | Packet.ConnectMeResponse (success, msg) -> state |> handle_connect_me_response success msg
  | Packet.AboutMe player -> state |> handle_about_me player
  | Packet.DisconnectMeResponse (success, msg) -> state |> handle_disconnect_me_response success msg
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
  let* () = Packet.send oc Packet.ConnectMe in
  (* At this point we expect the server to respond with an `AboutMe` packet *)
  let rec wait_for_about_me () =
    let* packet = Lwt_stream.get server_instream in
    match packet with
    | Some (Packet.AboutMe player) -> Lwt.return player
    | _ -> wait_for_about_me ()
  in
  let* player = wait_for_about_me () in
  let initial_state =
    { ui = Windows.create_windows ()
    ; log = []
    ; chat = Textbox.empty_edit
    ; player
    ; mode = Types.World
    ; send_packets = []
    }
    |> Input.add_log "Welcome to the horrifying world of Mooncaml *caml noises*."
  in
  game_loop initial_state ic oc
;;
