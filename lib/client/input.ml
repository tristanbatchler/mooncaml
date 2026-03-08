open Mooncaml_shared

let max_log = 256
let add_log msg (state : Types.state) = { state with log = Util.take max_log (msg :: state.log) }
let is_printable ch = ch >= Char.code ' ' && ch <= Char.code '~'

let handle_game_input (state : Types.state) ch =
  let move dx dy =
    let x = state.player_x + dx in
    let y = state.player_y + dy in
    { state with
      player_x = x
    ; player_y = y
    ; send_packets = Packet.Move (x, y) :: state.send_packets
    }
  in
  match ch with
  | c when c = Curses.Key.up -> move 0 (-1)
  | c when c = Curses.Key.down -> move 0 1
  | c when c = Curses.Key.left -> move (-1) 0
  | c when c = Curses.Key.right -> move 1 0
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
          send_packets = Packet.Say ed.text :: state.send_packets
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
