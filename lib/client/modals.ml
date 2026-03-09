let draw_choice_menu win_h win_w title options selected =
  let num_options = List.length options in
  let height = num_options + 4 in
  (* Find the longest string to set the window width *)
  let max_opt_len = List.fold_left (fun acc s -> max acc (String.length s)) 0 options in
  let width = max (String.length title + 4) (max_opt_len + 6) in
  (* Calculate absolute center of the terminal *)
  let start_y = (win_h - height) / 2 in
  let start_x = (win_w - width) / 2 in
  let win = Curses.newwin height width start_y start_x in
  Curses.box win 0 0;
  (* Center the title dynamically *)
  let title_pad = (width - String.length title - 2) / 2 in
  ignore (Curses.mvwaddstr win 0 title_pad (" " ^ title ^ " "));
  List.iteri
    (fun i text ->
       if i = selected then Curses.wattron win Curses.A.reverse;
       ignore (Curses.mvwaddstr win (i + 2) 2 (if i = selected then "> " ^ text else "  " ^ text));
       if i = selected then Curses.wattroff win Curses.A.reverse)
    options;
  ignore (Curses.wnoutrefresh win);
  ignore (Curses.delwin win)
;;

let draw_message_box win_h win_w title message =
  let lines = String.split_on_char '\n' message in
  let height = List.length lines + 6 in
  let max_line_len = List.fold_left (fun acc s -> max acc (String.length s)) 0 lines in
  let width = max (String.length title + 4) (max max_line_len 6 + 4) in
  let start_y = (win_h - height) / 2 in
  let start_x = (win_w - width) / 2 in
  let win = Curses.newwin height width start_y start_x in
  Curses.box win 0 0;
  let title_pad = (width - String.length title - 2) / 2 in
  ignore (Curses.mvwaddstr win 0 title_pad (" " ^ title ^ " "));
  List.iteri (fun i line -> ignore (Curses.mvwaddstr win (i + 2) 2 line)) lines;
  (* Draw OK button centered at the bottom *)
  let ok_label = "[ OK ]" in
  let ok_col = (width - String.length ok_label) / 2 in
  let ok_row = height - 3 in
  Curses.wattron win Curses.A.reverse;
  ignore (Curses.mvwaddstr win ok_row ok_col ok_label);
  Curses.wattroff win Curses.A.reverse;
  ignore (Curses.wnoutrefresh win);
  ignore (Curses.delwin win)
;;
