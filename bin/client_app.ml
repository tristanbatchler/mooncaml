(** Client application — roguelike UI skeleton using OCaml curses bindings. *)

(* ── Constants ──────────────────────────────────────────────────── *)

let log_height = 8
let chat_height = 1
let max_log = 256

(* ── Types ──────────────────────────────────────────────────────── *)

type input_mode =
  | Game
  | Chat

type ui =
  { map_win : Curses.window
  ; log_win : Curses.window
  ; chat_win : Curses.window
  ; width : int
  ; height : int
  }

type edit_line =
  { text : string
  ; cursor : int
  }

type state =
  { ui : ui
  ; log : string list
  ; chat : edit_line
  ; player_x : int
  ; player_y : int
  ; mode : input_mode
  }

(* ── Helpers ────────────────────────────────────────────────────── *)

let take n lst =
  let rec go n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go n [] lst
;;

let add_log msg state = { state with log = take max_log (msg :: state.log) }
let is_printable ch = ch >= Char.code ' ' && ch <= Char.code '~'

(* ── Log ────────────────────────────────────────────────────────── *)
let edit_clear _ed = { text = ""; cursor = 0 }

let edit_insert ed ch =
  let c = ed.cursor in
  let before = String.sub ed.text 0 c in
  let after = String.sub ed.text c (String.length ed.text - c) in
  { text = before ^ String.make 1 (Char.chr ch) ^ after; cursor = c + 1 }
;;

let edit_backspace ed =
  let c = ed.cursor in
  if c > 0
  then (
    let before = String.sub ed.text 0 (c - 1) in
    let after = String.sub ed.text c (String.length ed.text - c) in
    { text = before ^ after; cursor = c - 1 })
  else ed
;;

let edit_delete ed =
  let c = ed.cursor in
  let len = String.length ed.text in
  if c < len
  then (
    let before = String.sub ed.text 0 c in
    let after = String.sub ed.text (c + 1) (len - c - 1) in
    { ed with text = before ^ after })
  else ed
;;

let handle_game_input state ch =
  let next_state =
    match ch with
    | c when c = Curses.Key.up -> { state with player_y = state.player_y - 1 }
    | c when c = Curses.Key.down -> { state with player_y = state.player_y + 1 }
    | c when c = Curses.Key.left -> { state with player_x = state.player_x - 1 }
    | c when c = Curses.Key.right -> { state with player_x = state.player_x + 1 }
    | c when c = Curses.Key.enter || c = Char.code '\n' ->
      { state with mode = Chat; chat = edit_clear state.chat }
    | _ -> state
  in
  if ch <> Curses.Key.enter && ch <> Char.code '\n'
  then
    next_state |> add_log (Printf.sprintf "Moved to %d,%d" next_state.player_x next_state.player_y)
  else next_state
;;

let handle_chat_input state ch =
  let ed = state.chat in
  match ch with
  | c when c = Curses.Key.enter || c = Char.code '\n' ->
    let state = if String.length ed.text > 0 then add_log ed.text state else state in
    { state with mode = Game; chat = edit_clear ed }
  | c when c = Char.code '\x1b' -> { state with mode = Game; chat = edit_clear ed }
  | c when c = Curses.Key.backspace || c = Char.code '\x7f' ->
    { state with chat = edit_backspace ed }
  | c when c = Curses.Key.dc -> { state with chat = edit_delete ed }
  | c when c = Curses.Key.left -> { state with chat = { ed with cursor = max 0 (ed.cursor - 1) } }
  | c when c = Curses.Key.right ->
    { state with chat = { ed with cursor = min (String.length ed.text) (ed.cursor + 1) } }
  | c when c = Curses.Key.home -> { state with chat = { ed with cursor = 0 } }
  | c when c = Curses.Key.end_ -> { state with chat = { ed with cursor = String.length ed.text } }
  | c when is_printable c -> { state with chat = edit_insert ed c }
  | _ -> state
;;

(* ── Windows ────────────────────────────────────────────────────── *)

let destroy_windows ui =
  ignore (Curses.delwin ui.map_win);
  ignore (Curses.delwin ui.log_win);
  ignore (Curses.delwin ui.chat_win)
;;

let create_windows () =
  let h, w = Curses.getmaxyx (Curses.stdscr ()) in
  let map_h = h - log_height - chat_height in
  let map_win = Curses.newwin map_h w 0 0 in
  let log_win = Curses.newwin log_height w map_h 0 in
  let chat_win = Curses.newwin chat_height w (map_h + log_height) 0 in
  Curses.scrollok log_win true;
  ignore (Curses.keypad chat_win true);
  { map_win; log_win; chat_win; width = w; height = h }
;;

let handle_resize state =
  let h, w = Curses.get_size () in
  if h <> state.ui.height || w <> state.ui.width
  then (
    Curses.endwin ();
    ignore (Curses.refresh ());
    destroy_windows state.ui;
    Curses.clear ();
    ignore (Curses.refresh ());
    { state with ui = create_windows () })
  else state
;;

(* ── Drawing ────────────────────────────────────────────────────── *)

let draw_map state =
  Curses.werase state.ui.map_win;
  Curses.box state.ui.map_win 0 0;
  ignore (Curses.mvwaddch state.ui.map_win state.player_y state.player_x (Char.code '@'));
  ignore (Curses.wrefresh state.ui.map_win)
;;

let draw_log state =
  let w = state.ui.log_win in
  Curses.werase w;
  Curses.box w 0 0;
  let visible_msgs = take (log_height - 2) state.log |> List.rev in
  List.iteri (fun i msg -> ignore (Curses.mvwaddstr w (i + 1) 1 msg)) visible_msgs;
  ignore (Curses.wrefresh w)
;;

let draw_chat state =
  let w = state.ui.chat_win in
  let prompt = "> " in
  let prompt_len = String.length prompt in
  Curses.werase w;
  (match state.mode with
   | Game ->
     ignore (Curses.mvwaddstr w 0 0 "Press ENTER to chat");
     ignore (Curses.curs_set 0)
   | Chat ->
     ignore (Curses.mvwaddstr w 0 0 (prompt ^ state.chat.text));
     Curses.wclrtoeol w;
     ignore (Curses.wmove w 0 (prompt_len + state.chat.cursor));
     ignore (Curses.curs_set 1));
  ignore (Curses.wrefresh w)
;;

(* ── Main ───────────────────────────────────────────────────────── *)

let () =
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
    { ui = create_windows ()
    ; log = []
    ; chat = edit_clear { text = ""; cursor = 0 }
    ; player_x = 10
    ; player_y = 5
    ; mode = Game
    }
    |> add_log "Welcome to the roguelike UI skeleton."
  in
  Curses.timeout 50;
  let rec main_loop state =
    let state = handle_resize state in
    draw_map state;
    draw_log state;
    draw_chat state;
    let ch = Curses.getch () in
    let next_state =
      if ch <> -1 && ch <> Curses.Key.resize
      then (
        match state.mode with
        | Chat -> handle_chat_input state ch
        | Game -> handle_game_input state ch)
      else state
    in
    main_loop next_state
  in
  main_loop initial_state
;;
