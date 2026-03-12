type user_row =
  { id : int
  ; username : string
  ; password_hash : string
  }

type entity_row =
  { id : int
  ; map_name : string
  ; x : int
  ; y : int
  }

type player_row =
  { user_id : int
  ; entity_id : int
  ; display_name : string
  }
