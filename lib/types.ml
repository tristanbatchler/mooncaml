type input_mode =
  | World
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
