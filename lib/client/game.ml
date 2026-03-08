open Lwt.Syntax
open Mooncaml_shared

let server_instream, server_outstream = Lwt_stream.create ()
let handle_say state msg = state |> Input.add_log ("Someone said: " ^ msg)

let handle_say_response state success msg =
  if success
  then state |> Input.add_log ("You said: " ^ msg)
  else state |> Input.add_log ("Failed to say: " ^ msg)
;;

let handle_move state (x, y) = Types.{ state with player_x = x; player_y = y }

let handle_move_response state success msg =
  if success then state else state |> Input.add_log ("Failed to move: " ^ msg)
;;

let handle_disconnect state = state |> Input.add_log "Disconnected from server."
let handle_server_error state msg = state |> Input.add_log ("Server error: " ^ msg)

let handle_packet (state : Types.state) packet =
  match packet with
  | Packet.Say msg -> handle_say state msg
  | Packet.SayResponse (success, msg) -> handle_say_response state success msg
  | Packet.Move (x, y) -> handle_move state (x, y)
  | Packet.MoveResponse (success, msg) -> handle_move_response state success msg
  | Packet.Disconnect -> handle_disconnect state
  | Packet.ServerError msg -> handle_server_error state msg
;;

let run_todo _state oc = function
  | Types.SendPacket packet ->
    let* () = Logs_lwt.debug (fun m -> m "Sending packet: %s" (Packet.string_of_packet packet)) in
    Lwt_io.write_line oc (Packet.string_of_packet packet)
  | Types.Nothing -> Lwt.return_unit
;;

let rec game_loop state todo ic oc =
  let incoming = Lwt_stream.get_available server_instream in
  let state = Windows.handle_resize @@ List.fold_left handle_packet state incoming in
  let* () = run_todo state oc todo in
  Drawing.draw_map state;
  Drawing.draw_log state;
  Drawing.draw_chat state;
  let ch = Curses.getch () in
  let next_state, todo =
    if ch <> -1 && ch <> Curses.Key.resize
    then (
      match state.mode with
      | Chat -> Input.handle_chat_input state ch
      | World -> Input.handle_game_input state ch)
    else state, Types.Nothing
  in
  let* () = Lwt.pause () in
  game_loop next_state todo ic oc
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
    }
    |> Input.add_log "Welcome to the roguelike UI skeleton."
  in
  Curses.timeout 50;
  game_loop initial_state Types.Nothing ic oc
;;
