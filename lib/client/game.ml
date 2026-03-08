let run () =
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
    { ui = Windows.create_windows ()
    ; log = []
    ; chat = Textbox.empty_edit
    ; player_x = 10
    ; player_y = 5
    ; mode = Types.World
    }
    |> Input.add_log "Welcome to the roguelike UI skeleton."
  in
  Curses.timeout 50;
  let rec main_loop state =
    let state = Windows.handle_resize state in
    Drawing.draw_map state;
    Drawing.draw_log state;
    Drawing.draw_chat state;
    let ch = Curses.getch () in
    let next_state =
      if ch <> -1 && ch <> Curses.Key.resize
      then (
        match state.mode with
        | Chat -> Input.handle_chat_input state ch
        | World -> Input.handle_game_input state ch)
      else state
    in
    main_loop next_state
  in
  main_loop initial_state
;;
