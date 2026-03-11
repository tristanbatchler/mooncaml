open Mooncaml_shared

let cell_width = 2
let color_pair_player = 1
let color_pair_other_player = 2
let color_pair_grass = 3
let color_pair_dirt = 4
let color_pair_wall = 5
let color_pair_water = 6
let color_pair_border_focused = 7
let color_pair_border_unfocused = 8
let colors_ready = ref false

let ensure_colors () =
  if not !colors_ready
  then (
    if Curses.has_colors ()
    then (
      ignore (Curses.start_color ());
      ignore (Curses.use_default_colors ());
      ignore (Curses.init_pair color_pair_player (-1) (-1));
      ignore (Curses.init_pair color_pair_other_player Curses.Color.magenta (-1));
      ignore (Curses.init_pair color_pair_grass Curses.Color.green (-1));
      ignore (Curses.init_pair color_pair_dirt Curses.Color.yellow (-1));
      ignore (Curses.init_pair color_pair_wall (-1) (-1));
      ignore (Curses.init_pair color_pair_water Curses.Color.blue (-1));
      ignore (Curses.init_pair color_pair_border_focused Curses.Color.cyan (-1));
      ignore (Curses.init_pair color_pair_border_unfocused (-1) (-1)));
    colors_ready := true)
;;

let logo_art =
  [ {|      .-.                                                 .; |}
  ; {|      .;|/:                                             .;'  |}
  ; {|      .;   : .-.   .-.  . ,';. .-.   .-.    . ,';.,';.  .;   |}
  ; {|     .;    :;   ;';   ;';;  ;;;     ;   :   ;;  ;;  ;; ::    |}
  ; {| .:'.;     :`;;'  `;;' ';  ;; `;;;;'`:::'-'';  ;;  ';_;;_.-  |}
  ; {|(__.'      `.          ;    `.            _;        `-'      |}
  ]
;;

let draw_title_screen win_h win_w =
  let w = Curses.stdscr () in
  Curses.werase w;
  let logo_w = String.length (List.hd logo_art) in
  let start_y = (win_h / 2) - 8 in
  let start_x = (win_w - logo_w) / 2 in
  List.iteri (fun i line -> ignore (Curses.mvwaddstr w (start_y + i) start_x line)) logo_art;
  ignore (Curses.wnoutrefresh w)
;;

let draw_border win is_focused =
  let pair = if is_focused then color_pair_border_focused else color_pair_border_unfocused in
  let attr = Curses.A.color_pair pair in
  Curses.wattron win attr;
  Curses.box win 0 0;
  Curses.wattroff win attr
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

let hash_coord col row =
  (* Some large primes *)
  let p1 = 101051
  and p2 = 5001743
  and p3 = 1274126177 in
  (* Fancy hash and scramble to reduce linear patterns *)
  let h = col * p1 lxor (row * p2) in
  let h = h lxor (h lsr 13) * p3 in
  abs (h lxor (h lsr 16)) mod 1000
;;

let terrain_char terrain col row =
  let h = hash_coord col row in
  match terrain with
  | Maps.Grass -> if h < 750 then ',' else if h < 900 then ';' else '\''
  | Maps.Dirt -> if h < 600 then '.' else if h < 850 then '`' else ':'
  | Maps.Wall -> if h < 900 then '#' else if h < 950 then '[' else ']'
  | Maps.Water -> if h < 850 then '~' else '-'
  | Maps.OutOfBounds -> ' '
;;

let draw_terrain (state : Types.game_state) =
  ensure_colors ();
  let w = state.ui.map_win in
  let win_h, win_w = Curses.getmaxyx w in
  let view_cols = win_w - 2 in
  let view_rows = win_h - 2 in
  let cells_w = view_cols / cell_width in
  let cells_h = view_rows in
  Curses.werase w;
  draw_border w (state.focus = MapWindow);
  let indicator = Printf.sprintf "Mooncaml :: %s" state.map.name in
  let col = 1 in
  let attr =
    if state.focus = MapWindow
    then Curses.A.color_pair color_pair_border_focused
    else Curses.A.color_pair color_pair_border_unfocused
  in
  Curses.wattron w attr;
  ignore (Curses.mvwaddstr w 0 col indicator);
  Curses.wattroff w attr;
  let cam_x = state.player.x - (cells_w / 2) in
  let cam_y = state.player.y - (cells_h / 2) in
  let draw_cell cx cy =
    let mx = cam_x + cx in
    let my = cam_y + cy in
    if mx >= 0 && mx < state.map.width && my >= 0 && my < state.map.height
    then (
      match state.map.terrain_map.(my).(mx) with
      | OutOfBounds -> ()
      | terrain ->
        let pair =
          match terrain with
          | Grass -> color_pair_grass
          | Dirt -> color_pair_dirt
          | Wall -> color_pair_wall
          | Water -> color_pair_water
          | OutOfBounds -> assert false
        in
        let glyph =
          String.init cell_width (fun i ->
            let col = (mx * cell_width) + i in
            let row = my in
            terrain_char terrain col row)
        in
        let attr = Curses.A.color_pair pair in
        Curses.wattron w attr;
        ignore (Curses.mvwaddstr w (cy + 1) ((cx * cell_width) + 1) glyph);
        Curses.wattroff w attr)
  in
  let rec row cy =
    if cy < cells_h
    then (
      let rec col cx =
        if cx < cells_w
        then (
          draw_cell cx cy;
          col (cx + 1))
      in
      col 0;
      row (cy + 1))
  in
  row 0
;;

let draw_players (state : Types.game_state) =
  let w = state.ui.map_win in
  let win_h, win_w = Curses.getmaxyx w in
  let view_cols = win_w - 2 in
  let view_rows = win_h - 2 in
  let cells_w = view_cols / cell_width in
  let cells_h = view_rows in
  let cam_x = state.player.x - (cells_w / 2) in
  let cam_y = state.player.y - (cells_h / 2) in
  let draw_entity color map_x map_y glyph =
    let cx = map_x - cam_x in
    let cy = map_y - cam_y in
    if cx >= 0 && cx < cells_w && cy >= 0 && cy < cells_h
    then (
      let attr = Curses.A.color_pair color in
      Curses.wattron w attr;
      ignore (Curses.mvwaddstr w (cy + 1) ((cx * cell_width) + 1) glyph);
      Curses.wattroff w attr)
  in
  Types.IntMap.iter
    (fun _ (other : Entities.player) -> draw_entity color_pair_other_player other.x other.y " O")
    state.other_players;
  draw_entity color_pair_player state.player.x state.player.y " @"
;;

let draw_map (state : Types.game_state) =
  draw_terrain state;
  draw_players state;
  ignore (Curses.wnoutrefresh state.ui.map_win)
;;

let draw_log (state : Types.game_state) =
  let w = state.ui.log_win in
  let _, win_w = Curses.getmaxyx w in
  let inner_w = max 1 (win_w - 2) in
  let avail_rows = Windows.log_height - 2 in
  Curses.werase w;
  draw_border w (state.focus = LogWindow);
  let log_len = List.length state.log in
  let max_offset = max 0 (log_len - avail_rows) in
  let offset = min (max 0 state.log_scroll_offset) max_offset in
  let scrolled_log =
    let rec drop n lst =
      if n <= 0
      then lst
      else (
        match lst with
        | [] -> []
        | _ :: rest -> drop (n - 1) rest)
    in
    drop offset state.log
  in
  let rec collect acc remaining = function
    | ([] | _ :: _) when remaining <= 0 -> acc
    | [] -> acc
    | msg :: rest ->
      let lines = wrap_text inner_w msg in
      let n = List.length lines in
      if n <= remaining
      then collect (lines :: acc) (remaining - n) rest
      else (
        let tail = List.rev lines |> Util.take remaining |> List.rev in
        tail :: acc)
  in
  let visible_lines = collect [] avail_rows scrolled_log |> List.concat in
  List.iteri (fun i line -> ignore (Curses.mvwaddstr w (i + 1) 1 line)) visible_lines;
  let percent =
    if offset = 0
    then "Bot"
    else if offset = max_offset
    then "Top"
    else
      Printf.sprintf
        "%d%%"
        (100 - int_of_float (100. *. float_of_int offset /. float_of_int (max 1 max_offset)))
  in
  let indicator = Printf.sprintf "[%s]" percent in
  let col = win_w - String.length indicator - 1 in
  let row = Windows.log_height - 1 in
  let attr =
    if state.focus = LogWindow
    then Curses.A.color_pair color_pair_border_focused
    else Curses.A.color_pair color_pair_border_unfocused
  in
  Curses.wattron w attr;
  ignore (Curses.mvwaddstr w row col indicator);
  Curses.wattroff w attr;
  ignore (Curses.wnoutrefresh w)
;;

let draw_chat (state : Types.game_state) =
  let w = state.ui.chat_win in
  let prompt = "> " in
  let prompt_len = String.length prompt in
  Curses.werase w;
  (match state.focus with
   | ChatWindow ->
     ignore (Curses.mvwaddstr w 0 0 (prompt ^ state.chat.text));
     Curses.wclrtoeol w;
     ignore (Curses.wmove w 0 (prompt_len + state.chat.cursor));
     ignore (Curses.curs_set 1)
   | _ ->
     ignore (Curses.mvwaddstr w 0 0 (Printf.sprintf "%s says: " state.player.name));
     ignore (Curses.curs_set 0));
  ignore (Curses.wnoutrefresh w)
;;

(* --- Master render function --- *)
let draw_app (state : Types.client_state) =
  let h, w = Curses.get_size () in
  ignore (Curses.curs_set 0);
  (* Hide cursor by default *)
  let active_popup =
    match state.mode with
    | Title t ->
      draw_title_screen h w;
      t.popup
    | Game g ->
      draw_terrain g;
      draw_players g;
      ignore (Curses.wnoutrefresh g.ui.map_win);
      draw_log g;
      draw_chat g;
      g.popup
  in
  (* Popups render on top of EVERYTHING *)
  (match active_popup with
   | Types.NoPopup -> ()
   | Types.ChoiceMenu { title; options; selected; _ } ->
     Modals.draw_choice_menu h w title options selected
   | Types.MessageBox { title; message } -> Modals.draw_message_box h w title message
   | Types.Form { title; fields; cursor; _ } -> Modals.draw_form h w title fields cursor);
  ignore (Curses.doupdate ())
;;
