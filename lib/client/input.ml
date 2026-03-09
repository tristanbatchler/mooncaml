open Mooncaml_shared

let src = Logs.Src.create "mooncaml_client.input" ~doc:"Input handling for the game client"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let max_log = 256

let add_log msg (state : Types.state) =
  let log = Util.take max_log (msg :: state.log) in
  (* If scrolled up, bump offset so the visible content stays frozen *)
  let offset = if state.log_scroll_offset > 0 then state.log_scroll_offset + 1 else 0 in
  { state with log; log_scroll_offset = offset }
;;

let add_logf fmt = Printf.ksprintf (fun msg state -> add_log msg state) fmt
let is_printable ch = ch >= Char.code ' ' && ch <= Char.code '~'
let is_return_key c = c = Curses.Key.enter || c = Char.code '\n'
let is_escape_key c = c = Char.code '\x1b'
let is_cursor_up_key c = c = Curses.Key.up || c = Char.code 'k'
let is_cursor_down_key c = c = Curses.Key.down || c = Char.code 'j'

(* ----- From other clients (forwarded by the server) -------------------------- *)

let move_player dx dy (state : Types.state) =
  let desired_x = state.player.x + dx in
  let desired_y = state.player.y + dy in
  if desired_x < 0 || desired_y < 0 || desired_x >= state.map.width || desired_y >= state.map.height
  then state
  else (
    let terrain = state.map.terrain_map.(desired_y).(desired_x) in
    let can_move =
      match terrain with
      | Grass | Dirt -> true
      | _ -> false
    in
    if not can_move
    then state
    else
      { state with
        player = { state.player with x = desired_x; y = desired_y }
      ; send_packets = Packet.MoveCommand { x = desired_x; y = desired_y } :: state.send_packets
      })
;;

let next_focus = function
  | Types.MapWindow -> Types.LogWindow
  | Types.LogWindow -> Types.ChatWindow
  | Types.ChatWindow -> Types.MapWindow
;;

let prev_focus = function
  | Types.MapWindow -> Types.ChatWindow
  | Types.LogWindow -> Types.MapWindow
  | Types.ChatWindow -> Types.LogWindow
;;

let handle_map_input (state : Types.state) ch =
  match ch with
  | c when c = Curses.Key.up -> state |> move_player 0 (-1)
  | c when c = Curses.Key.down -> state |> move_player 0 1
  | c when c = Curses.Key.left -> state |> move_player (-1) 0
  | c when c = Curses.Key.right -> state |> move_player 1 0
  | c when is_return_key c -> { state with focus = ChatWindow; chat = Textbox.empty_edit }
  | _ -> state
;;

let handle_log_input (state : Types.state) ch =
  let log_len = List.length state.log in
  let avail_rows = Windows.log_height - 2 in
  let max_offset = max 0 (log_len - avail_rows) in
  let offset = state.log_scroll_offset in
  match ch with
  | c when is_cursor_up_key c ->
    let next = min max_offset (offset + 1) in
    { state with log_scroll_offset = next }
  | c when is_cursor_down_key c ->
    let next = max 0 (offset - 1) in
    { state with log_scroll_offset = next }
  | c when c = Char.code 'G' -> { state with log_scroll_offset = 0 }
  | c when c = Char.code 'g' -> { state with log_scroll_offset = max_offset }
  | c when is_return_key c -> { state with focus = ChatWindow; chat = Textbox.empty_edit }
  | _ ->
    let clamped = min max_offset (max 0 offset) in
    { state with log_scroll_offset = clamped }
;;

let handle_chat_input (state : Types.state) ch =
  let ed = state.chat in
  let just_edit ed' = { state with chat = ed' } in
  match ch with
  | c when is_return_key c ->
    let state =
      if String.length ed.text > 0
      then
        { (add_log ("You said: " ^ ed.text) state) with
          send_packets = Packet.ChatCommand ed.text :: state.send_packets
        }
      else state
    in
    { state with focus = Types.MapWindow; chat = Textbox.empty_edit }
  | c when is_escape_key c -> { state with focus = Types.MapWindow; chat = Textbox.empty_edit }
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

let confirm_quit_game (state : Types.state) =
  { state with
    popup =
      Types.ChoiceMenu
        { title = "Really quit?"
        ; options = [ "No, keep playing"; "Get me outta here!!" ]
        ; selected = 0
        ; id = Types.MenuConfirmQuit
        }
  }
;;

let handle_menu_choice (state : Types.state) id selected_index =
  match id with
  | Types.MenuQuit ->
    if selected_index = 1 then confirm_quit_game state else { state with popup = NoPopup }
  | Types.MenuConfirmQuit -> if selected_index = 1 then exit 0 else { state with popup = NoPopup }
  | Types.MenuInspect -> state (* Placeholder for future menu *)
;;

let handle_input (state : Types.state) ch =
  match state.popup with
  | Types.ChoiceMenu { title; options; selected; id } ->
    let max_idx = List.length options - 1 in
    (match ch with
     | c when is_cursor_up_key c ->
       { state with popup = ChoiceMenu { title; options; id; selected = max 0 (selected - 1) } }
     | c when is_cursor_down_key c ->
       { state with
         popup = ChoiceMenu { title; options; id; selected = min max_idx (selected + 1) }
       }
     | c when is_return_key c -> handle_menu_choice state id selected
     | c when is_escape_key c -> { state with popup = NoPopup }
     | _ -> state)
  | Types.MessageBox _ ->
    if is_escape_key ch || is_return_key ch then { state with popup = NoPopup } else state
  | Types.NoPopup ->
    if ch = Char.code '\t'
    then { state with focus = next_focus state.focus }
    else if ch = Curses.Key.btab
    then { state with focus = prev_focus state.focus }
    else if is_escape_key ch && state.focus <> Types.ChatWindow
    then
      (* Spawn our generic quit menu! *)
      { state with
        popup =
          Types.ChoiceMenu
            { title = "Quit Game?"
            ; options = [ "Resume"; "Quit" ]
            ; selected = 0
            ; id = Types.MenuQuit
            }
      }
    else (
      match state.focus with
      | MapWindow -> handle_map_input state ch
      | LogWindow -> handle_log_input state ch
      | ChatWindow -> handle_chat_input state ch)
;;
