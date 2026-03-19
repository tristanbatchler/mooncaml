open Caqti_request.Infix

module Schema = struct
  let create_users_if_not_exists =
    (Caqti_type.unit ->. Caqti_type.unit)
    @@ {sql|
      CREATE TABLE IF NOT EXISTS users (
        id SERIAL PRIMARY KEY,
        username VARCHAR(50) UNIQUE NOT NULL,
        password_hash VARCHAR(255) NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
        last_login_at TIMESTAMP WITH TIME ZONE,
        last_update_at TIMESTAMP WITH TIME ZONE DEFAULT NULL
      );
    |sql}
  ;;

  let create_entities_if_not_exists =
    (Caqti_type.unit ->. Caqti_type.unit)
    @@ {sql|
      CREATE TABLE IF NOT EXISTS entities (
        id SERIAL PRIMARY KEY,
        map_name VARCHAR(50) NOT NULL,
        x INTEGER NOT NULL,
        y INTEGER NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
        last_update_at TIMESTAMP WITH TIME ZONE DEFAULT NULL
      );
    |sql}
  ;;

  let create_entities_maps_index_if_not_exists =
    (Caqti_type.unit ->. Caqti_type.unit)
    @@ {sql|
      CREATE INDEX IF NOT EXISTS idx_entities_map ON entities(map_name);
    |sql}
  ;;

  let create_players_if_not_exists =
    (Caqti_type.unit ->. Caqti_type.unit)
    @@ {sql|
      CREATE TABLE IF NOT EXISTS players (
        user_id INTEGER PRIMARY KEY REFERENCES users(id) ON DELETE CASCADE,
        entity_id INTEGER UNIQUE REFERENCES entities(id) ON DELETE CASCADE,
        display_name VARCHAR(50) NOT NULL
      );
    |sql}
  ;;
end

let get_psql_version =
  (Caqti_type.unit ->! Caqti_type.string)
  @@ {sql|
    SELECT version();
  |sql}
;;

let get_user_opt_by_username =
  (Caqti_type.string ->? Caqti_type.(t2 int string))
  @@ {sql|
    SELECT id, password_hash
    FROM users
    WHERE username = $1;
  |sql}
;;

let create_user =
  (Caqti_type.(t2 string string) ->! Caqti_type.int)
  @@ {sql|
    INSERT INTO users (username, password_hash)
    VALUES ($1, $2)
    RETURNING id;
  |sql}
;;

let create_entity =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
  @@ {sql|
    INSERT INTO entities (map_name, x, y)
    VALUES ($1, $2, $3)
    RETURNING id;
  |sql}
;;

let get_player_by_user_id =
  (Caqti_type.int ->? Caqti_type.(t2 int string))
  @@ {sql|
    SELECT e.id, p.display_name
    FROM players p
    JOIN entities e ON p.entity_id = e.id
    WHERE p.user_id = $1;
  |sql}
;;

let create_player =
  (* Replaced the broken t5 CTE with a standard t3 insert *)
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
  @@ {sql|
    INSERT INTO players (user_id, entity_id, display_name)
    VALUES ($1, $2, $3);
  |sql}
;;

let update_player_position =
  (Caqti_type.(t3 int int int) ->. Caqti_type.unit)
  @@ {sql|
    UPDATE entities
    SET x = $2,
        y = $3,
        last_update_at = CURRENT_TIMESTAMP
    WHERE id = $1;
  |sql}
;;

let get_player_position_by_user_id =
  (Caqti_type.int ->? Caqti_type.(t2 int int))
  @@ {sql|
    SELECT e.x, e.y
    FROM players p
    JOIN entities e ON p.entity_id = e.id
    WHERE p.user_id = $1;
  |sql}
;;
