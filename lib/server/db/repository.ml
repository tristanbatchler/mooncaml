open Lwt.Syntax

let create_schema pool =
  let* res =
    Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
      let* _ = Db.exec Queries.Schema.create_users_if_not_exists () in
      let* _ = Db.exec Queries.Schema.create_entities_if_not_exists () in
      let* _ = Db.exec Queries.Schema.create_entities_maps_index_if_not_exists () in
      let* _ = Db.exec Queries.Schema.create_players_if_not_exists () in
      Lwt.return (Ok ()))
  in
  Lwt.return (Util.unwrap_result res)
;;

let get_user_opt_by_username pool username =
  let* res =
    Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find_opt Queries.get_user_opt_by_username username)
  in
  match Util.unwrap_result res with
  | Ok (Some (id, password_hash)) -> Lwt.return (Ok (Some (id, username, password_hash)))
  | Ok None -> Lwt.return (Ok None)
  | Error msg -> Lwt.return (Error msg)
;;

let create_user pool username password_hash =
  let* res =
    Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find Queries.create_user (username, password_hash))
  in
  Lwt.return (Util.unwrap_result res)
;;

let get_player_opt_by_user_id pool user_id =
  let* res =
    Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find_opt Queries.get_player_by_user_id user_id)
  in
  match Util.unwrap_result res with
  | Ok (Some (entity_id, x, y, display_name)) ->
    Lwt.return (Ok (Some (entity_id, x, y, display_name)))
  | Ok None -> Lwt.return (Ok None)
  | Error msg -> Lwt.return (Error msg)
;;

let create_player pool user_id map_name x y display_name =
  let* res =
    Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
      (* Step 1: Insert Entity *)
      let* entity_id_res = Db.find Queries.create_entity (map_name, x, y) in
      match entity_id_res with
      | Ok entity_id ->
        (* Step 2: Insert Player using the returned entity_id *)
        Db.exec Queries.create_player (user_id, entity_id, display_name)
      | Error e -> Lwt.return (Error e))
  in
  Lwt.return (Util.unwrap_result res)
;;

let update_player_position pool entity_id x y =
  let* res =
    Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.exec Queries.update_player_position (entity_id, x, y))
  in
  Lwt.return (Util.unwrap_result res)
;;
