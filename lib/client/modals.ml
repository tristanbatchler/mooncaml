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
       ignore (Curses.mvwaddstr win (i + 2) 2 text);
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
  let ok_label = "OK" in
  let ok_col = (width - String.length ok_label) / 2 in
  let ok_row = height - 3 in
  Curses.wattron win Curses.A.reverse;
  ignore (Curses.mvwaddstr win ok_row ok_col ok_label);
  Curses.wattroff win Curses.A.reverse;
  ignore (Curses.wnoutrefresh win);
  ignore (Curses.delwin win)
;;

let draw_form win_h win_w title fields cursor =
  let num_fields = Array.length fields in
  let height = num_fields + 6 in
  let max_label_len =
    Array.fold_left (fun acc f -> max acc (String.length f.Types.label)) 0 fields
  in
  let width = max (String.length title + 4) (max_label_len + 24) in
  let start_y = (win_h - height) / 2 in
  let start_x = (win_w - width) / 2 in
  let win = Curses.newwin height width start_y start_x in
  Curses.box win 0 0;
  let title_pad = (width - String.length title - 2) / 2 in
  ignore (Curses.mvwaddstr win 0 title_pad (" " ^ title ^ " "));
  (* Track absolute screen coordinates for the hardware cursor *)
  let hw_cursor_y = ref (-1) in
  let hw_cursor_x = ref (-1) in
  Array.iteri
    (fun i (field : Types.form_field) ->
       let row = i + 2 in
       let col = 2 in
       let display_text =
         if field.is_secret
         then String.make (String.length field.edit.text) '*'
         else field.edit.text
       in
       if i = cursor then Curses.wattron win Curses.A.bold;
       ignore
         (Curses.mvwaddstr
            win
            row
            col
            (Printf.sprintf "%-*s : %s" max_label_len field.label display_text));
       if i = cursor then Curses.wattroff win Curses.A.bold;
       if i = cursor
       then (
         hw_cursor_y := start_y + row;
         hw_cursor_x := start_x + col + max_label_len + 3 + field.edit.cursor))
    fields;
  (* Draw Submit Button (Index = num_fields) *)
  let submit_idx = num_fields in
  let submit_label = "Submit" in
  let submit_row = height - 3 in
  let submit_col = 2 in
  if cursor = submit_idx then Curses.wattron win Curses.A.reverse;
  ignore (Curses.mvwaddstr win submit_row submit_col submit_label);
  if cursor = submit_idx then Curses.wattroff win Curses.A.reverse;
  (* Draw Cancel Button (Index = num_fields + 1) *)
  let cancel_idx = num_fields + 1 in
  let cancel_label = "Cancel" in
  let cancel_col = width - String.length cancel_label - 2 in
  if cursor = cancel_idx then Curses.wattron win Curses.A.reverse;
  ignore (Curses.mvwaddstr win submit_row cancel_col cancel_label);
  if cursor = cancel_idx then Curses.wattroff win Curses.A.reverse;
  ignore (Curses.wnoutrefresh win);
  ignore (Curses.delwin win);
  (* Set the global hardware cursor *)
  if !hw_cursor_y <> -1
  then (
    ignore (Curses.move !hw_cursor_y !hw_cursor_x);
    ignore (Curses.curs_set 1))
  else ignore (Curses.curs_set 0)
;;
