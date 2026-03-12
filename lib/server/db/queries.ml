open Caqti_request.Infix
open Lwt.Syntax

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

module Queries = struct
  let get_psql_version =
    (Caqti_type.unit ->! Caqti_type.string)
    @@ {sql|
      SELECT version();
    |sql}
  ;;

  let get_user_opt_by_username =
    (Caqti_type.string ->? Caqti_type.(t2 int string))
    @@ {sql|
      SELECT id, password_hash FROM users WHERE username = $1
    |sql}
  ;;

  let create_user =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    @@ {sql|
      INSERT INTO users (username, password_hash) VALUES ($1, $2)
    |sql}
  ;;

  let create_entity =
    (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    @@ {sql|
      INSERT INTO entities (map_name, x, y) VALUES ($1, $2, $3) RETURNING id
    |sql}
  ;;

  let get_player_by_user_id =
    (Caqti_type.int ->? Caqti_type.(t3 int int string))
    @@ {sql|
      SELECT p.user_id, e.id, p.display_name
      FROM players p
      JOIN entities e ON p.entity_id = e.id
      WHERE p.user_id = $1
    |sql}
  ;;

  let get_entity_by_id =
    (Caqti_type.int ->? Caqti_type.(t3 string int int))
    @@ {sql|
      SELECT map_name, x, y FROM entities WHERE id = $1
    |sql}
  ;;

  let create_player =
    (Caqti_type.(t5 int string int int string) ->. Caqti_type.unit)
    @@ {sql|
      WITH new_entity AS (
        INSERT INTO entities (map_name, x, y) VALUES ($2, $3, $4) RETURNING id
      )
      INSERT INTO players (user_id, entity_id, display_name)
      VALUES ($1, (SELECT id FROM new_entity), $5)
    |sql}
  ;;
end

let create_schema pool =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    let* _ = Db.exec Schema.create_users_if_not_exists () in
    let* _ = Db.exec Schema.create_entities_if_not_exists () in
    let* _ = Db.exec Schema.create_entities_maps_index_if_not_exists () in
    let* _ = Db.exec Schema.create_players_if_not_exists () in
    Lwt.return (Ok ()))
;;

let get_psql_version pool =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find Queries.get_psql_version ())
;;

let get_user_opt_by_username pool username =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Queries.get_user_opt_by_username username)
  |> Lwt.map (function
    | Ok (Ok (Some (id, password_hash))) -> Some Models.{ id; username; password_hash }
    | _ -> None)
;;

let create_user pool username password_hash =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Queries.create_user (username, password_hash))
;;

let get_player_opt_by_user_id pool user_id =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Queries.get_player_by_user_id user_id)
  |> Lwt.map (function
    | Ok (Ok (Some (user_id, entity_id, display_name))) ->
      Some Models.{ user_id; entity_id; display_name }
    | _ -> None)
;;

let create_player pool user_id map_name x y display_name =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Queries.create_player (user_id, map_name, x, y, display_name))
;;
