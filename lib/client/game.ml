open Lwt.Syntax
open Mooncaml_shared

let server_instream, server_outstream = Lwt_stream.create ()

let handle_says_other state sender_id msg =
  Input.add_log (Printf.sprintf "Client %d says: %s" sender_id msg) state
;;

let handle_move_other state sender_id x y =
  Input.add_log (Printf.sprintf "Client %d moves to (%d, %d)" sender_id x y) state
;;

let handle_disconnect_other state sender_id =
  Input.add_log (Printf.sprintf "Client %d has disconnected" sender_id) state
;;

(* ----- From the server directly -------------------------- *)

let handle_unexpected_server_error state msg = Input.add_log ("Server error: " ^ msg) state

let handle_says_me_response state success msg =
  if success then state else state |> Input.add_log ("Failed to send message: " ^ msg)
;;

let handle_move_me_response state success msg =
  if success then state else state |> Input.add_log ("Failed to move: " ^ msg)
;;

let handle_disconnect_me_response state success msg =
  if success then state else state |> Input.add_log ("Failed to disconnect: " ^ msg)
;;

let handle_packet (state : Types.state) packet =
  match packet with
  (* From another client (forwarded by the server) *)
  | Packet.SaysOther (sender_id, msg) -> handle_says_other state sender_id msg
  | Packet.MoveOther (sender_id, x, y) -> handle_move_other state sender_id x y
  | Packet.DisconnectOther sender_id -> handle_disconnect_other state sender_id
  (* From the server directly *)
  | Packet.UnexpectedServerError msg -> handle_unexpected_server_error state msg
  | Packet.SaysMeResponse (success, msg) -> handle_says_me_response state success msg
  | Packet.MoveMeResponse (success, msg) -> handle_move_me_response state success msg
  | Packet.DisconnectMeResponse (success, msg) -> handle_disconnect_me_response state success msg
  | _ -> state
;;

let rec send_out_packets oc = function
  | [] -> Lwt.return_unit
  | packet :: rest ->
    let* () = Logs_lwt.debug (fun m -> m "Sending packet: %s" (Packet.string_of_packet packet)) in
    let* () = Lwt_io.write_line oc (Packet.string_of_packet packet) in
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
  let initial_state =
    { ui = Windows.create_windows ()
    ; log = []
    ; chat = Textbox.empty_edit
    ; player_x = 10
    ; player_y = 5
    ; mode = Types.World
    ; send_packets = []
    }
    |> Input.add_log "Welcome to the roguelike UI skeleton."
  in
  Curses.timeout 50;
  game_loop initial_state ic oc
;;
