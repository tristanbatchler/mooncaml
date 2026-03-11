let log_height = 8
let chat_height = 1

let destroy_windows (ui : Types.game_ui) =
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
  Types.{ map_win; log_win; chat_win; width = w; height = h }
;;

let handle_resize (ui : Types.game_ui) =
  let h, w = Curses.get_size () in
  if h <> ui.height || w <> ui.width
  then (
    Curses.endwin ();
    ignore (Curses.refresh ());
    destroy_windows ui;
    Curses.clear ();
    ignore (Curses.refresh ());
    create_windows ())
  else ui
;;
