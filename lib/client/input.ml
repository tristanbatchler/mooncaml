open Mooncaml_shared

let max_log = 256
let add_log msg (state : Types.state) = { state with log = Util.take max_log (msg :: state.log) }
let add_logf fmt = Printf.ksprintf (fun msg state -> add_log msg state) fmt
let is_printable ch = ch >= Char.code ' ' && ch <= Char.code '~'

let move_player dx dy (state : Types.state) =
  let desired_x = state.player.x + dx in
  let desired_y = state.player.y + dy in
  if desired_x < 0 || desired_y < 0 || desired_x >= state.map.width || desired_y >= state.map.height
  then state
  else (
    let terrain = state.map.terrain_map.(desired_y).(desired_x) in
    let can_move =
      match terrain with
      | Maps.Grass | Maps.Dirt -> true
      | _ -> false
    in
    if not can_move
    then state
    else
      { state with
        (* Optimistic prediction: update player coordinates immediately *)
        player = { state.player with x = desired_x; y = desired_y }
      ; send_packets = Packet.MoveCommand { x = desired_x; y = desired_y } :: state.send_packets
      })
;;

let handle_game_input (state : Types.state) ch =
  match ch with
  | c when c = Curses.Key.up -> state |> move_player 0 (-1)
  | c when c = Curses.Key.down -> state |> move_player 0 1
  | c when c = Curses.Key.left -> state |> move_player (-1) 0
  | c when c = Curses.Key.right -> state |> move_player 1 0
  | c when c = Curses.Key.enter || c = Char.code '\n' ->
    { state with mode = Chat; chat = Textbox.empty_edit }
  | _ -> state
;;

let handle_chat_input (state : Types.state) ch =
  let ed = state.chat in
  let just_edit ed' = { state with chat = ed' } in
  match ch with
  | c when c = Curses.Key.enter || c = Char.code '\n' ->
    let state =
      if String.length ed.text > 0
      then
        { (add_log ("You said: " ^ ed.text) state) with
          send_packets = Packet.ChatCommand ed.text :: state.send_packets
        }
      else state
    in
    { state with mode = Types.World; chat = Textbox.empty_edit }
  | c when c = Char.code '\x1b' -> { state with mode = Types.World; chat = Textbox.empty_edit }
  | c when c = Curses.Key.backspace || c = Char.code '\x7f' -> just_edit (Textbox.edit_backspace ed)
  | c when c = Curses.Key.dc -> just_edit (Textbox.edit_delete ed)
  | c when c = Curses.Key.left -> just_edit { ed with cursor = max 0 (ed.cursor - 1) }
  | c when c = Curses.Key.right ->
    just_edit { ed with cursor = min (String.length ed.text) (ed.cursor + 1) }
  | c when c = Curses.Key.home -> just_edit { ed with cursor = 0 }
  | c when c = Curses.Key.end_ -> just_edit { ed with cursor = String.length ed.text }
  | c when is_printable c -> just_edit (Textbox.edit_insert ed c)
  | _ -> state
;;
