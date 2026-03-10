open Mooncaml_server
open Mooncaml_shared
open Lwt.Syntax

let setup_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logging.custom_reporter ())
;;

let get_env ?default key =
  match Sys.getenv_opt key with
  | Some value -> value
  | None ->
    (match default with
     | Some d -> d
     | None ->
       Logs.err (fun m -> m "Environment variable %s not set and no default provided" key);
       failwith ("Missing environment variable: " ^ key))
;;

let db_config : Mooncaml_server_db.Types.config =
  let open Mooncaml_server_db.Types in
  { host = get_env "DB_HOST" ~default:"127.0.0.1"
  ; port = get_env "DB_PORT" ~default:"5432" |> int_of_string
  ; database = get_env "DB_NAME"
  ; user = get_env "DB_USER"
  ; password = get_env "DB_PASS"
  ; pool_size = get_env "DB_POOL_SIZE" ~default:"10" |> int_of_string
  }
;;

let init_db_and_start_server port =
  Logs.info (fun m -> m "Initializing database connection pool...");
  let* pool_result = Mooncaml_server_db.Util.create_pool db_config in
  match pool_result with
  | Error err ->
    let err_msg =
      match err with
      | `Connection_error e -> e
      | _ -> "Unknown error"
    in
    Logs.err (fun m -> m "FATAL: Failed to connect to database: %s" err_msg);
    Lwt.fail_with "Database initialization failed"
  | Ok pool -> Server.start port pool
;;

let () =
  setup_logging ();
  Logs.info (fun m -> m "Booting up...");
  Lwt_main.run @@ init_db_and_start_server 43216
;;
