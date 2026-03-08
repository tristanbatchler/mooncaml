open Types

let take n lst =
  let rec go n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go n [] lst
;;

let draw_map (state : state) =
  Curses.werase state.ui.map_win;
  Curses.box state.ui.map_win 0 0;
  ignore (Curses.mvwaddch state.ui.map_win state.player_y state.player_x (Char.code '@'));
  ignore (Curses.wrefresh state.ui.map_win)
;;

let draw_log (state : state) =
  let w = state.ui.log_win in
  Curses.werase w;
  Curses.box w 0 0;
  let visible_msgs = take (Windows.log_height - 2) state.log |> List.rev in
  List.iteri (fun i msg -> ignore (Curses.mvwaddstr w (i + 1) 1 msg)) visible_msgs;
  ignore (Curses.wrefresh w)
;;

let draw_chat (state : state) =
  let w = state.ui.chat_win in
  let prompt = "> " in
  let prompt_len = String.length prompt in
  Curses.werase w;
  (match state.mode with
   | World ->
     ignore (Curses.mvwaddstr w 0 0 "Press ENTER to chat");
     ignore (Curses.curs_set 0)
   | Chat ->
     ignore (Curses.mvwaddstr w 0 0 (prompt ^ state.chat.text));
     Curses.wclrtoeol w;
     ignore (Curses.wmove w 0 (prompt_len + state.chat.cursor));
     ignore (Curses.curs_set 1));
  ignore (Curses.wrefresh w)
;;
