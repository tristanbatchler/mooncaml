open Caqti_request.Infix
open Lwt.Syntax

module Queries = struct
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
end

let create_schema pool =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    let* _ = Db.exec Queries.create_users_if_not_exists () in
    let* _ = Db.exec Queries.create_entities_if_not_exists () in
    let* _ = Db.exec Queries.create_entities_maps_index_if_not_exists () in
    let* _ = Db.exec Queries.create_players_if_not_exists () in
    Lwt.return (Ok ()))
;;

let get_psql_version pool =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find Queries.get_psql_version ())
;;

let get_user_opt_by_username pool username =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Queries.get_user_opt_by_username username)
;;

let create_user pool username password_hash =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Queries.create_user (username, password_hash))
;;
