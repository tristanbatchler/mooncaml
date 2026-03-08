let draw_map (state : Types.state) =
  Curses.werase state.ui.map_win;
  Curses.box state.ui.map_win 0 0;
  (* +1 offset so game coord (0,0) maps to window coord (1,1), inside the border *)
  ignore
    (Curses.mvwaddch state.ui.map_win (state.player.y + 1) (state.player.x + 1) (Char.code '@'));
  ignore (Curses.wrefresh state.ui.map_win)
;;

(* Split a message into lines that fit within [width] columns *)
let wrap_text width msg =
  let len = String.length msg in
  if len = 0
  then [ "" ]
  else (
    let rec go acc pos =
      if pos >= len
      then List.rev acc
      else (
        let chunk_len = min width (len - pos) in
        go (String.sub msg pos chunk_len :: acc) (pos + chunk_len))
    in
    go [] 0)
;;

let draw_log (state : Types.state) =
  let w = state.ui.log_win in
  let _, win_w = Curses.getmaxyx w in
  let inner_w = max 1 (win_w - 2) in
  let avail_rows = Windows.log_height - 2 in
  Curses.werase w;
  Curses.box w 0 0;
  (* state.log is newest-first.  Collect wrapped line-groups from newest to
     oldest, prepending each group so List.concat yields oldest-first order. *)
  let rec collect acc remaining = function
    | ([] | _ :: _) when remaining <= 0 -> acc
    | [] -> acc
    | msg :: rest ->
      let lines = wrap_text inner_w msg in
      let n = List.length lines in
      if n <= remaining
      then collect (lines :: acc) (remaining - n) rest
      else (
        (* Partially visible: show the last [remaining] lines *)
        let tail = List.rev lines |> Util.take remaining |> List.rev in
        tail :: acc)
  in
  let visible_lines = collect [] avail_rows state.log |> List.concat in
  List.iteri (fun i line -> ignore (Curses.mvwaddstr w (i + 1) 1 line)) visible_lines;
  ignore (Curses.wrefresh w)
;;

let draw_chat (state : Types.state) =
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
