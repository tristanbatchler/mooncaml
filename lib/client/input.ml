open Mooncaml_shared

let src = Logs.Src.create "mooncaml_client.input" ~doc:"Input handling for the game client"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let max_log = 256

let add_log msg (state : Types.game_state) =
  let log = Util.take max_log (msg :: state.log) in
  (* If scrolled up, bump offset so the visible content stays frozen *)
  let offset = if state.log_scroll_offset > 0 then state.log_scroll_offset + 1 else 0 in
  { state with log; log_scroll_offset = offset }
;;

let add_logf fmt = Printf.ksprintf (fun msg state -> add_log msg state) fmt
let is_printable ch = ch >= Char.code ' ' && ch <= Char.code '~'
let is_return_key c = c = Curses.Key.enter || c = Char.code '\n'
let is_escape_key c = c = Char.code '\x1b'

(* ----- From other clients (forwarded by the server) -------------------------- *)

let move_player dx dy (state : Types.game_state) =
  let desired_x = state.player.x + dx in
  let desired_y = state.player.y + dy in
  if desired_x < 0 || desired_y < 0 || desired_x >= state.map.width || desired_y >= state.map.height
  then state
  else (
    match state.map.terrain_map.(desired_y).(desired_x) with
    | Grass | Dirt -> { state with player = { state.player with x = desired_x; y = desired_y } }
    | _ -> state)
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

let handle_map_input (state : Types.game_state) ch =
  match ch with
  | c when c = Curses.Key.up -> state |> move_player 0 (-1)
  | c when c = Curses.Key.down -> state |> move_player 0 1
  | c when c = Curses.Key.left -> state |> move_player (-1) 0
  | c when c = Curses.Key.right -> state |> move_player 1 0
  | c when is_return_key c -> { state with focus = ChatWindow; chat = Textbox.empty_edit }
  | _ -> state
;;

let handle_log_input (state : Types.game_state) ch =
  let is_cursor_up_key c = c = Curses.Key.up || c = Char.code 'k' in
  let is_cursor_down_key c = c = Curses.Key.down || c = Char.code 'j' in
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

let handle_chat_input (state : Types.game_state) ch =
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

let confirm_quit_game (state : Types.game_state) =
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

let update_form_field (fields : Types.form_field array) cursor f =
  let field = fields.(cursor) in
  let new_fields = Array.copy fields in
  new_fields.(cursor) <- { field with edit = f field.edit };
  new_fields
;;

let handle_form_submit (c_state : Types.client_state) id (fields : Types.form_field array) =
  let username = fields.(0).edit.text in
  let password = fields.(1).edit.text in
  let packet =
    match id with
    | Types.FormLogin -> Packet.LoginCommand { username; password }
    | Types.FormRegister -> Packet.RegisterCommand { username; password }
  in
  { c_state with send_packets = packet :: c_state.send_packets }
;;

let handle_menu_choice (state : Types.game_state) id selected_index =
  match id with
  | Types.MenuQuit ->
    if selected_index = 1 then confirm_quit_game state else { state with popup = NoPopup }
  | Types.MenuConfirmQuit -> if selected_index = 1 then exit 0 else { state with popup = NoPopup }
  | Types.MenuInspect -> state (* Placeholder for future menu *)
  | Types.MenuTitle -> state (* Placeholder for future menu *)
;;

let handle_popup_input (c_state : Types.client_state) popup ch =
  let open Types in
  let is_cursor_up_key c = c = Curses.Key.up in
  let is_cursor_down_key c = c = Curses.Key.down in
  match popup with
  | ChoiceMenu { title; options; selected; id } ->
    let max_idx = List.length options - 1 in
    (match ch with
     | c when is_cursor_up_key c || c = Curses.Key.btab ->
       ChoiceMenu { title; options; id; selected = max 0 (selected - 1) }, c_state.send_packets
     | c when is_cursor_down_key c || c = Char.code '\t' ->
       ( ChoiceMenu { title; options; id; selected = min max_idx (selected + 1) }
       , c_state.send_packets )
     | c when is_return_key c ->
       if id = MenuTitle && selected = 0
       then
         ( Form
             { title = "Login"
             ; id = FormLogin
             ; cursor = 0
             ; fields =
                 [| { label = "User"; is_secret = false; edit = Textbox.empty_edit }
                  ; { label = "Pass"; is_secret = true; edit = Textbox.empty_edit }
                 |]
             }
         , [] )
       else if id = MenuTitle && selected = 1
       then
         ( Form
             { title = "Register"
             ; id = FormRegister
             ; cursor = 0
             ; fields =
                 [| { label = "User"; is_secret = false; edit = Textbox.empty_edit }
                  ; { label = "Pass"; is_secret = true; edit = Textbox.empty_edit }
                 |]
             }
         , [] )
       else if selected = 2
       then exit 0
       else NoPopup, []
     | _ -> popup, c_state.send_packets)
  | Form { title; fields; cursor; id } ->
    let num_fields = Array.length fields in
    let max_cursor = num_fields + 1 in
    (* 0=User, 1=Pass, 2=Submit, 3=Cancel *)
    (match ch with
     | c when is_cursor_up_key c || c = Curses.Key.btab ->
       Form { title; fields; id; cursor = max 0 (cursor - 1) }, []
     | c when is_cursor_down_key c || c = Char.code '\t' ->
       Form { title; fields; id; cursor = min max_cursor (cursor + 1) }, []
     (* --- NEW: Left/Right Button Navigation --- *)
     | c when c = Curses.Key.left && cursor >= num_fields ->
       (* If on Cancel (3), move to Submit (2) *)
       Form { title; fields; id; cursor = num_fields }, []
     | c when c = Curses.Key.right && cursor >= num_fields ->
       (* If on Submit (2), move to Cancel (3) *)
       Form { title; fields; id; cursor = num_fields + 1 }, []
       (* ----------------------------------------- *)
     | c when is_return_key c ->
       if cursor < num_fields
       then
         (* Pressing Enter on a textbox moves to the next field *)
         Form { title; fields; id; cursor = cursor + 1 }, []
       else if cursor = num_fields
       then
         (* Pressing Enter on Submit Button! *)
         NoPopup, (handle_form_submit c_state id fields).send_packets
       else
         (* Pressing Enter on Cancel Button! *)
         ( ChoiceMenu
             { title = "Mooncaml"
             ; options = [ "Login"; "Register"; "Quit" ]
             ; selected = 0
             ; id = MenuTitle
             }
         , [] )
     | c when is_escape_key c ->
       ( ChoiceMenu
           { title = "Mooncaml"
           ; options = [ "Login"; "Register"; "Quit" ]
           ; selected = 0
           ; id = MenuTitle
           }
       , [] )
     | c when cursor < num_fields ->
       (* Text editing only applies if we are highlighting a textbox! *)
       let apply f = Form { title; fields = update_form_field fields cursor f; cursor; id }, [] in
       if c = Curses.Key.backspace || c = Char.code '\x7f'
       then apply Textbox.edit_backspace
       else if c = Curses.Key.dc
       then apply Textbox.edit_delete
       else if c = Curses.Key.left
       then apply (fun e -> { e with cursor = max 0 (e.cursor - 1) })
       else if c = Curses.Key.right
       then apply (fun e -> { e with cursor = min (String.length e.text) (e.cursor + 1) })
       else if c = Curses.Key.home
       then apply (fun e -> { e with cursor = 0 })
       else if c = Curses.Key.end_
       then apply (fun e -> { e with cursor = String.length e.text })
       else if is_printable c
       then apply (fun e -> Textbox.edit_insert e c)
       else popup, []
     | _ -> popup, [])
  | MessageBox _ -> if is_escape_key ch || is_return_key ch then NoPopup, [] else popup, []
  | NoPopup -> NoPopup, []
;;

let handle_input (state : Types.client_state) ch =
  match state.mode with
  | Title t ->
    let new_popup, new_packets = handle_popup_input state t.popup ch in
    Types.{ mode = Title { popup = new_popup }; send_packets = new_packets }
  | Game g ->
    if g.popup <> NoPopup
    then (
      let new_popup, new_packets = handle_popup_input state g.popup ch in
      Types.{ mode = Game { g with popup = new_popup }; send_packets = new_packets })
    else (
      let new_g =
        if ch = Char.code '\t'
        then { g with focus = next_focus g.focus }
        else if ch = Curses.Key.btab
        then { g with focus = prev_focus g.focus }
        else if is_escape_key ch && g.focus <> ChatWindow
        then confirm_quit_game g
        else (
          match g.focus with
          | MapWindow -> handle_map_input g ch
          | LogWindow -> handle_log_input g ch
          | ChatWindow -> handle_chat_input g ch)
      in
      (* Merge game packets into state packets *)
      Types.{ mode = Game new_g; send_packets = new_g.send_packets @ state.send_packets })
;;
