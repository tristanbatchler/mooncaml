open Mooncaml_shared

let color_pair_player = 1
let color_pair_other_player = 2
let color_pair_grass = 3
let color_pair_dirt = 4
let color_pair_wall = 5
let color_pair_water = 6
let colors_ready = ref false

let ensure_colors () =
  if not !colors_ready
  then (
    if Curses.has_colors ()
    then (
      ignore (Curses.start_color ());
      ignore (Curses.use_default_colors ());
      ignore (Curses.init_pair color_pair_player Curses.Color.white (-1));
      ignore (Curses.init_pair color_pair_other_player Curses.Color.magenta (-1));
      ignore (Curses.init_pair color_pair_grass Curses.Color.green (-1));
      ignore (Curses.init_pair color_pair_dirt Curses.Color.yellow (-1));
      ignore (Curses.init_pair color_pair_wall Curses.Color.white (-1));
      ignore (Curses.init_pair color_pair_water Curses.Color.blue (-1)));
    colors_ready := true)
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

let draw_terrain (state : Types.state) =
  ensure_colors ();
  let w = state.ui.map_win in
  Curses.werase w;
  Curses.box w 0 0;
  for y = 0 to state.map.height - 1 do
    for x = 0 to state.map.width - 1 do
      match state.map.terrain_map.(y).(x) with
      | OutOfBounds -> ()
      | terrain ->
        let pseudorandom_10 = ((x * 31) + (y * 17)) mod 10 in
        let glyph, pair =
          match terrain with
          | Grass -> (if pseudorandom_10 < 7 then "," else ";"), color_pair_grass
          | Dirt -> (if pseudorandom_10 < 5 then "." else "`"), color_pair_dirt
          | Wall -> "#", color_pair_wall
          | Water -> (if pseudorandom_10 < 5 then "~" else "-"), color_pair_water
          | OutOfBounds -> assert false
        in
        let attr = Curses.A.color_pair pair in
        Curses.wattron w attr;
        ignore (Curses.mvwaddstr w (y + 1) (x + 1) glyph);
        Curses.wattroff w attr
    done
  done
;;

let draw_players (state : Types.state) =
  (* +1 offset so game coord (0,0) maps to window coord (1,1), inside the border *)
  let attr = Curses.A.color_pair color_pair_player in
  Curses.wattron state.ui.map_win attr;
  ignore
    (Curses.mvwaddch state.ui.map_win (state.player.y + 1) (state.player.x + 1) (Char.code '@'));
  Curses.wattroff state.ui.map_win attr;
  let attr = Curses.A.color_pair color_pair_other_player in
  Curses.wattron state.ui.map_win attr;
  Types.IntMap.iter
    (fun _ (other : Entities.player) ->
       ignore (Curses.mvwaddch state.ui.map_win (other.y + 1) (other.x + 1) (Char.code 'O')))
    state.other_players;
  Curses.wattroff state.ui.map_win attr
;;

let draw_map (state : Types.state) =
  draw_terrain state;
  draw_players state;
  ignore (Curses.wrefresh state.ui.map_win)
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
