open Mooncaml_shared
module IntMap = Map.Make (Int)

type window_focus =
  | MapWindow
  | LogWindow
  | ChatWindow

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
  ; player : Entities.player
  ; focus : window_focus
  ; log_scroll_offset : int
  ; send_packets : Packet.t list
  ; other_players : Entities.player IntMap.t
  ; map : Maps.map_data
  }
