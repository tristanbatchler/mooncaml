open Lwt.Syntax

let unwrap = function
  | Ok x -> Ok x
  | _ ->
    Logs.err (fun m -> m "Database error");
    Error "Database error"
;;

let create_schema pool =
  let* res =
    pool
    |> Caqti_lwt_unix.Pool.use (fun (module Db : Caqti_lwt.CONNECTION) ->
      let* _ = Db.exec Queries.Schema.create_users_if_not_exists () in
      let* _ = Db.exec Queries.Schema.create_entities_if_not_exists () in
      let* _ = Db.exec Queries.Schema.create_entities_maps_index_if_not_exists () in
      let* _ = Db.exec Queries.Schema.create_players_if_not_exists () in
      Lwt.return (Ok ()))
  in
  Lwt.return (unwrap res)
;;

let get_user_opt_by_username pool username =
  let* res =
    pool
    |> Caqti_lwt_unix.Pool.use (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find_opt Queries.get_user_opt_by_username username)
  in
  Lwt.return
  @@
  match unwrap res with
  | Ok (Some (id, password_hash)) -> Ok (Some Models.{ id; username; password_hash })
  | Ok None -> Ok None
  | Error msg -> Error msg
;;

let create_user pool username password_hash =
  let* res =
    pool
    |> Caqti_lwt_unix.Pool.use (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find Queries.create_user (username, password_hash))
  in
  Lwt.return (unwrap res)
;;

let get_player_opt_by_user_id pool user_id =
  let* res =
    pool
    |> Caqti_lwt_unix.Pool.use (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find_opt Queries.get_player_by_user_id user_id)
  in
  Lwt.return
  @@
  match unwrap res with
  | Ok (Some (entity_id, display_name)) -> Ok (Some Models.{ user_id; entity_id; display_name })
  | Ok None -> Ok None
  | Error msg -> Error msg
;;

let create_player pool user_id map_name x y display_name =
  let* res =
    pool
    |> Caqti_lwt_unix.Pool.use (fun (module Db : Caqti_lwt.CONNECTION) ->
      let* entity_id_res = Db.find Queries.create_entity (map_name, x, y) in
      match entity_id_res with
      | Ok entity_id -> Db.exec Queries.create_player (user_id, entity_id, display_name)
      | Error e -> Lwt.return (Error e))
  in
  Lwt.return @@ unwrap res
;;

let update_player_position pool entity_id x y =
  let* res =
    pool
    |> Caqti_lwt_unix.Pool.use (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.exec Queries.update_player_position (entity_id, x, y))
  in
  Lwt.return @@ unwrap res
;;

let get_player_position pool entity_id =
  let* res =
    pool
    |> Caqti_lwt_unix.Pool.use (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find_opt Queries.get_player_position_by_user_id entity_id)
  in
  Lwt.return
  @@
  match unwrap res with
  | Ok (Some (x, y)) -> Ok (x, y)
  | Ok None -> Error "Player not found"
  | Error msg -> Error msg
;;
