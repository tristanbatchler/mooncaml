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
  then state, []
  else (
    match state.map.terrain_map.(desired_y).(desired_x) with
    | Grass | Dirt ->
      ( { state with player = { state.player with x = desired_x; y = desired_y } }
      , [ Packet.MoveCommand { x = desired_x; y = desired_y } ] )
    | _ -> state, [])
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
  | c when c = Curses.Key.up -> move_player 0 (-1) state
  | c when c = Curses.Key.down -> move_player 0 1 state
  | c when c = Curses.Key.left -> move_player (-1) 0 state
  | c when c = Curses.Key.right -> move_player 1 0 state
  | c when is_return_key c -> { state with focus = ChatWindow; chat = Textbox.empty_edit }, []
  | _ -> state, []
;;

let handle_log_input (state : Types.game_state) ch =
  let is_cursor_up_key c = c = Curses.Key.up || c = Char.code 'k' in
  let is_cursor_down_key c = c = Curses.Key.down || c = Char.code 'j' in
  let log_len = List.length state.log in
  let avail_rows = Windows.log_height - 2 in
  let max_offset = max 0 (log_len - avail_rows) in
  let offset = state.log_scroll_offset in
  let state' =
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
  in
  state', []
;;

let handle_chat_input (state : Types.game_state) ch =
  let ed = state.chat in
  let just_edit ed' = { state with chat = ed' }, [] in
  match ch with
  | c when is_return_key c ->
    if String.length ed.text > 0
    then (
      let new_state =
        { (add_log ("You said: " ^ ed.text) state) with
          focus = Types.MapWindow
        ; chat = Textbox.empty_edit
        }
      in
      new_state, [ Packet.ChatCommand ed.text ])
    else { state with focus = Types.MapWindow; chat = Textbox.empty_edit }, []
  | c when is_escape_key c -> { state with focus = Types.MapWindow; chat = Textbox.empty_edit }, []
  | c when c = Curses.Key.backspace || c = Char.code '\x7f' -> just_edit (Textbox.edit_backspace ed)
  | c when c = Curses.Key.dc -> just_edit (Textbox.edit_delete ed)
  | c when c = Curses.Key.left -> just_edit { ed with cursor = max 0 (ed.cursor - 1) }
  | c when c = Curses.Key.right ->
    just_edit { ed with cursor = min (String.length ed.text) (ed.cursor + 1) }
  | c when c = Curses.Key.home -> just_edit { ed with cursor = 0 }
  | c when c = Curses.Key.end_ -> just_edit { ed with cursor = String.length ed.text }
  | c when is_printable c -> just_edit (Textbox.edit_insert ed c)
  | _ -> state, []
;;

let confirm_quit_game (state : Types.game_state) =
  { state with
    popup =
      Types.ChoiceMenu
        { title = "Mooncaml"
        ; options = [ "Resume"; "Logout"; "Quit to Desktop" ]
        ; selected = 0
        ; id = Types.MenuInGameEscape
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

(* Just create the packet, don't mutate state here to avoid duplication bugs! *)
let create_form_packet id (fields : Types.form_field array) =
  let username = fields.(0).edit.text in
  let password = fields.(1).edit.text in
  match id with
  | Types.FormLogin -> Packet.LoginCommand { username; password }
  | Types.FormRegister -> Packet.RegisterCommand { username; password }
;;

let title_popup =
  Types.ChoiceMenu
    { title = "Mooncaml"
    ; options = [ "Login"; "Register"; "Quit" ]
    ; selected = 0
    ; id = Types.MenuTitle
    }
;;

let handle_menu_choice (c_state : Types.client_state) id selected_index =
  match c_state.mode, id with
  | Types.Game g, Types.MenuInGameEscape ->
    if selected_index = 0
    then Types.{ c_state with mode = Game { g with popup = NoPopup } }
    else if selected_index = 1
    then (
      Windows.destroy_windows g.ui;
      Curses.clear ();
      ignore (Curses.refresh ());
      (* If you defined LogoutCommand, use it here! Otherwise DisconnectCommand works. *)
      let new_packets = Packet.DisconnectCommand :: c_state.send_packets in
      Types.{ mode = Title { popup = title_popup }; send_packets = new_packets })
    else exit 0
  | _, Types.MenuConfirmQuit -> if selected_index = 1 then exit 0 else c_state
  | _, _ -> c_state
;;

let handle_popup_input (c_state : Types.client_state) popup ch =
  let open Types in
  let is_cursor_up_key c = c = Curses.Key.up in
  let is_cursor_down_key c = c = Curses.Key.down in
  (* Safely update popup *)
  let set_popup p pkts =
    let mode =
      match c_state.mode with
      | Title _ -> Title { popup = p }
      | Game g -> Game { g with popup = p }
    in
    { mode; send_packets = pkts @ c_state.send_packets }
  in
  match popup with
  | ChoiceMenu { title; options; selected; id } ->
    let max_idx = List.length options - 1 in
    (match ch with
     | c when is_cursor_up_key c || c = Curses.Key.btab ->
       set_popup (ChoiceMenu { title; options; id; selected = max 0 (selected - 1) }) []
     | c when is_cursor_down_key c || c = Char.code '\t' ->
       set_popup (ChoiceMenu { title; options; id; selected = min max_idx (selected + 1) }) []
     | c when is_return_key c ->
       if id = MenuTitle && selected = 0
       then
         set_popup
           (Form
              { title = "Login"
              ; id = FormLogin
              ; cursor = 0
              ; fields =
                  [| { label = "User"; is_secret = false; edit = Textbox.empty_edit }
                   ; { label = "Pass"; is_secret = true; edit = Textbox.empty_edit }
                  |]
              })
           []
       else if id = MenuTitle && selected = 1
       then
         set_popup
           (Form
              { title = "Register"
              ; id = FormRegister
              ; cursor = 0
              ; fields =
                  [| { label = "User"; is_secret = false; edit = Textbox.empty_edit }
                   ; { label = "Pass"; is_secret = true; edit = Textbox.empty_edit }
                  |]
              })
           []
       else if id = MenuTitle && selected = 2
       then exit 0
       else handle_menu_choice c_state id selected
     | _ -> c_state)
  | Form { title; fields; cursor; id } ->
    let num_fields = Array.length fields in
    let max_cursor = num_fields + 1 in
    (match ch with
     | c when is_cursor_up_key c || c = Curses.Key.btab ->
       set_popup (Form { title; fields; id; cursor = max 0 (cursor - 1) }) []
     | c when is_cursor_down_key c || c = Char.code '\t' ->
       set_popup (Form { title; fields; id; cursor = min max_cursor (cursor + 1) }) []
     | c when c = Curses.Key.left && cursor >= num_fields ->
       set_popup (Form { title; fields; id; cursor = num_fields }) []
     | c when c = Curses.Key.right && cursor >= num_fields ->
       set_popup (Form { title; fields; id; cursor = num_fields + 1 }) []
     | c when is_return_key c ->
       if cursor < num_fields
       then set_popup (Form { title; fields; id; cursor = cursor + 1 }) []
       else if cursor = num_fields
       then (
         let packet = create_form_packet id fields in
         set_popup NoPopup [ packet ])
       else set_popup title_popup []
     | c when is_escape_key c -> set_popup title_popup []
     | c when cursor < num_fields ->
       let apply f =
         set_popup (Form { title; fields = update_form_field fields cursor f; cursor; id }) []
       in
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
       else c_state
     | _ -> c_state)
  | MessageBox _ ->
    if is_escape_key ch || is_return_key ch
    then (
      match c_state.mode with
      | Title _ -> set_popup title_popup []
      | Game _ -> set_popup NoPopup [])
    else c_state
  | NoPopup -> c_state
;;

let handle_input (state : Types.client_state) ch =
  match state.mode with
  | Title t -> handle_popup_input state t.popup ch
  | Game g ->
    if g.popup <> NoPopup
    then
      (* handle_popup_input now natively returns a fully formed client_state! *)
      handle_popup_input state g.popup ch
    else (
      let new_g, new_packets =
        if ch = Char.code '\t'
        then { g with focus = next_focus g.focus }, []
        else if ch = Curses.Key.btab
        then { g with focus = prev_focus g.focus }, []
        else if is_escape_key ch && g.focus <> ChatWindow
        then confirm_quit_game g, []
        else (
          match g.focus with
          | MapWindow -> handle_map_input g ch
          | LogWindow -> handle_log_input g ch
          | ChatWindow -> handle_chat_input g ch)
      in
      Types.{ mode = Game new_g; send_packets = new_packets @ state.send_packets })
;;
