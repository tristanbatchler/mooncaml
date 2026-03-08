open Mooncaml_shared

let max_log = 256

let take n lst =
  let rec go n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go n [] lst
;;

let add_log msg (state : Types.state) = { state with log = take max_log (msg :: state.log) }
let is_printable ch = ch >= Char.code ' ' && ch <= Char.code '~'

let handle_game_input (state : Types.state) ch =
  match ch with
  | c when c = Curses.Key.up ->
    ( { state with player_y = state.player_y - 1 }
    , Types.SendPacket (Packet.Move (state.player_x, state.player_y - 1)) )
  | c when c = Curses.Key.down ->
    ( { state with player_y = state.player_y + 1 }
    , Types.SendPacket (Packet.Move (state.player_x, state.player_y + 1)) )
  | c when c = Curses.Key.left ->
    ( { state with player_x = state.player_x - 1 }
    , Types.SendPacket (Packet.Move (state.player_x - 1, state.player_y)) )
  | c when c = Curses.Key.right ->
    ( { state with player_x = state.player_x + 1 }
    , Types.SendPacket (Packet.Move (state.player_x + 1, state.player_y)) )
  | c when c = Curses.Key.enter || c = Char.code '\n' ->
    { state with mode = Chat; chat = Textbox.empty_edit }, Types.Nothing
  | _ -> state, Types.Nothing
;;

let handle_chat_input (state : Types.state) ch =
  let ed = state.chat in
  match ch with
  | c when c = Curses.Key.enter || c = Char.code '\n' ->
    let state, todo =
      if String.length ed.text > 0
      then add_log ed.text state, Types.SendPacket (Packet.Say ed.text)
      else state, Types.Nothing
    in
    { state with mode = Types.World; chat = Textbox.empty_edit }, todo
  | c when c = Char.code '\x1b' ->
    { state with mode = Types.World; chat = Textbox.empty_edit }, Types.Nothing
  | c when c = Curses.Key.backspace || c = Char.code '\x7f' ->
    { state with chat = Textbox.edit_backspace ed }, Types.Nothing
  | c when c = Curses.Key.dc -> { state with chat = Textbox.edit_delete ed }, Types.Nothing
  | c when c = Curses.Key.left ->
    { state with chat = { ed with cursor = max 0 (ed.cursor - 1) } }, Types.Nothing
  | c when c = Curses.Key.right ->
    ( { state with chat = { ed with cursor = min (String.length ed.text) (ed.cursor + 1) } }
    , Types.Nothing )
  | c when c = Curses.Key.home -> { state with chat = { ed with cursor = 0 } }, Types.Nothing
  | c when c = Curses.Key.end_ ->
    { state with chat = { ed with cursor = String.length ed.text } }, Types.Nothing
  | c when is_printable c -> { state with chat = Textbox.edit_insert ed c }, Types.Nothing
  | _ -> state, Types.Nothing
;;
