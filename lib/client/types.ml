open Mooncaml_shared
module IntMap = Map.Make (Int)

type window_focus =
  | MapWindow
  | LogWindow
  | ChatWindow

type game_ui =
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

type form_id =
  | FormLogin
  | FormRegister

type form_field =
  { label : string
  ; is_secret : bool
  ; edit : edit_line
  }

type menu_id =
  | MenuTitle
  | MenuInGameEscape
  | MenuConfirmQuit
  | MenuInspect

type popup_state =
  | NoPopup
  | ChoiceMenu of
      { title : string
      ; options : string list
      ; selected : int
      ; id : menu_id
      }
  | MessageBox of
      { title : string
      ; message : string
      }
  | Form of
      { title : string
      ; fields : form_field array
      ; cursor : int
      ; id : form_id
      }

type title_state = { popup : popup_state }

type game_state =
  { ui : game_ui
  ; log : string list
  ; chat : edit_line
  ; player : Entities.player
  ; focus : window_focus
  ; log_scroll_offset : int
  ; other_players : Entities.player IntMap.t
  ; map : Maps.map_data
  ; popup : popup_state
  }

type app_mode =
  | Title of title_state
  | Game of game_state

type client_state =
  { mode : app_mode
  ; send_packets : Packet.t list
  }
