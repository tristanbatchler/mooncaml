open Lwt.Syntax
open Mooncaml_shared

let server_instream, server_outstream = Lwt_stream.create ()
let handle_say state msg = state |> Input.add_log ("Someone said: " ^ msg)

let handle_say_response state success msg =
  if success then state else state |> Input.add_log ("Failed to say: " ^ msg)
;;

let handle_move_response state success msg =
  if success then state else state |> Input.add_log ("Failed to move: " ^ msg)
;;

let handle_disconnect state = state |> Input.add_log "Disconnected from server."
let handle_server_error state msg = state |> Input.add_log ("Server error: " ^ msg)

let handle_packet (state : Types.state) packet =
  match packet with
  | Packet.Say msg -> handle_say state msg
  | Packet.SayResponse (success, msg) -> handle_say_response state success msg
  | Packet.MoveResponse (success, msg) -> handle_move_response state success msg
  | Packet.Disconnect -> handle_disconnect state
  | Packet.ServerError msg -> handle_server_error state msg
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
