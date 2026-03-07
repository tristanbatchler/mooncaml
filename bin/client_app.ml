(** Client application — roguelike UI skeleton using OCaml curses bindings. *)

(* ── Constants ──────────────────────────────────────────────────── *)

let log_height = 6
let chat_height = 1
let max_log = 256

(* ── Types ──────────────────────────────────────────────────────── *)

type input_mode =
  | Game
  | Chat

type ui =
  { mutable map_win : Curses.window
  ; mutable log_win : Curses.window
  ; mutable chat_win : Curses.window
  ; mutable width : int
  ; mutable height : int
  }

(** A simple editable line with a cursor position. *)
type edit_line =
  { mutable text : string
  ; mutable cursor : int (** Byte offset within [text]. *)
  }

type state =
  { ui : ui
  ; mutable log : string list (** Messages stored newest-first. *)
  ; chat : edit_line
  ; mutable player_x : int
  ; mutable player_y : int
  ; mutable mode : input_mode
  }

(* ── Helpers ────────────────────────────────────────────────────── *)

(** [take n lst] returns up to the first [n] elements of [lst]. *)
let take n lst =
  let rec go acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> go (x :: acc) (n - 1) xs
  in
  go [] n lst
;;

(* ── Log ────────────────────────────────────────────────────────── *)

let add_log state msg = state.log <- take max_log (msg :: state.log)

(* ── Windows ────────────────────────────────────────────────────── *)

let destroy_windows ui =
  ignore (Curses.delwin ui.map_win);
  ignore (Curses.delwin ui.log_win);
  ignore (Curses.delwin ui.chat_win)
;;

let create_windows ui =
  let h, w = Curses.getmaxyx (Curses.stdscr ()) in
  ui.height <- h;
  ui.width <- w;
  let map_h = h - log_height - chat_height in
  ui.map_win <- Curses.newwin map_h w 0 0;
  ui.log_win <- Curses.newwin log_height w map_h 0;
  ui.chat_win <- Curses.newwin chat_height w (map_h + log_height) 0;
  Curses.scrollok ui.log_win true;
  ignore (Curses.keypad ui.chat_win true)
;;

let handle_resize ui =
  let h, w = Curses.get_size () in
  if h <> ui.height || w <> ui.width
  then (
    Curses.endwin ();
    ignore (Curses.refresh ());
    destroy_windows ui;
    Curses.clear ();
    ignore (Curses.refresh ());
    create_windows ui)
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
  let visible = log_height - 2 in
  Curses.werase w;
  Curses.box w 0 0;
  let messages = take visible state.log |> List.rev in
  List.iteri (fun i msg -> ignore (Curses.mvwaddstr w (i + 1) 1 msg)) messages;
  ignore (Curses.wrefresh w)
;;

let chat_prompt = "> "
let chat_prompt_len = String.length chat_prompt

let draw_chat state =
  let w = state.ui.chat_win in
  Curses.werase w;
  (match state.mode with
   | Game ->
     ignore (Curses.mvwaddstr w 0 0 "Press ENTER to chat");
     ignore (Curses.curs_set 0)
   | Chat ->
     ignore (Curses.mvwaddstr w 0 0 (chat_prompt ^ state.chat.text));
     Curses.wclrtoeol w;
     ignore (Curses.wmove w 0 (chat_prompt_len + state.chat.cursor));
     ignore (Curses.curs_set 1));
  ignore (Curses.wrefresh w)
;;

(* ── Input handling ─────────────────────────────────────────────── *)

let is_printable ch = ch >= Char.code ' ' && ch <= Char.code '~'

let edit_clear ed =
  ed.text <- "";
  ed.cursor <- 0
;;

let edit_insert ed ch =
  let c = ed.cursor in
  let before = String.sub ed.text 0 c in
  let after = String.sub ed.text c (String.length ed.text - c) in
  ed.text <- before ^ String.make 1 (Char.chr ch) ^ after;
  ed.cursor <- c + 1
;;

let edit_backspace ed =
  let c = ed.cursor in
  if c > 0
  then (
    let before = String.sub ed.text 0 (c - 1) in
    let after = String.sub ed.text c (String.length ed.text - c) in
    ed.text <- before ^ after;
    ed.cursor <- c - 1)
;;

let edit_delete ed =
  let c = ed.cursor in
  let len = String.length ed.text in
  if c < len
  then (
    let before = String.sub ed.text 0 c in
    let after = String.sub ed.text (c + 1) (len - c - 1) in
    ed.text <- before ^ after)
;;

let handle_game_input state ch =
  if ch = Curses.Key.up
  then state.player_y <- state.player_y - 1
  else if ch = Curses.Key.down
  then state.player_y <- state.player_y + 1
  else if ch = Curses.Key.left
  then state.player_x <- state.player_x - 1
  else if ch = Curses.Key.right
  then state.player_x <- state.player_x + 1
  else if ch = Curses.Key.enter || ch = Char.code '\n'
  then (
    state.mode <- Chat;
    edit_clear state.chat);
  add_log state (Printf.sprintf "Moved to %d,%d" state.player_x state.player_y)
;;

let handle_chat_input state ch =
  let ed = state.chat in
  if ch = Curses.Key.enter || ch = Char.code '\n'
  then (
    if String.length ed.text > 0 then add_log state ed.text;
    state.mode <- Game;
    edit_clear ed)
  else if ch = Char.code '\x1b'
  then (
    state.mode <- Game;
    edit_clear ed)
  else if ch = Curses.Key.backspace || ch = Char.code '\x7f'
  then edit_backspace ed
  else if ch = Curses.Key.dc
  then edit_delete ed
  else if ch = Curses.Key.left
  then (if ed.cursor > 0 then ed.cursor <- ed.cursor - 1)
  else if ch = Curses.Key.right
  then (if ed.cursor < String.length ed.text then ed.cursor <- ed.cursor + 1)
  else if ch = Curses.Key.home
  then ed.cursor <- 0
  else if ch = Curses.Key.end_
  then ed.cursor <- String.length ed.text
  else if is_printable ch
  then edit_insert ed ch
;;

(* ── Main ───────────────────────────────────────────────────────── *)

let () =
  let _stdscr = Curses.initscr () in
  at_exit Curses.endwin;
  ignore (Curses.cbreak ());
  ignore (Curses.noecho ());
  ignore (Curses.keypad (Curses.stdscr ()) true);
  ignore (Curses.curs_set 0);
  Curses.winch_handler_on ();
  let ui =
    { map_win = Curses.null_window
    ; log_win = Curses.null_window
    ; chat_win = Curses.null_window
    ; width = 0
    ; height = 0
    }
  in
  create_windows ui;
  let state =
    { ui; log = []; chat = { text = ""; cursor = 0 }; player_x = 10; player_y = 5; mode = Game }
  in
  add_log state "Welcome to the roguelike UI skeleton.";
  ignore (Curses.refresh ());
  Curses.timeout 50;
  let rec main_loop () =
    handle_resize state.ui;
    draw_map state;
    draw_log state;
    draw_chat state;
    let ch = Curses.getch () in
    if ch <> -1 && ch <> Curses.Key.resize
    then (
      match state.mode with
      | Chat -> handle_chat_input state ch
      | Game -> handle_game_input state ch);
    main_loop ()
  in
  main_loop ()
;;
