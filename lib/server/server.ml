open Lwt.Syntax

let src = Logs.Src.create "mooncaml_server.server" ~doc:"Low-level server operations"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)
module Db = Mooncaml_server_db

let connection_handler client_addr (ic, oc) db_pool =
  let* () =
    match client_addr with
    | Unix.ADDR_INET (inet_addr, port) ->
      Log_lwt.info (fun m ->
        m "Accepted connection from %s:%d" (Unix.string_of_inet_addr inet_addr) port)
    | _ -> Log_lwt.err (fun m -> m "Rejected connection from unsupported address type")
  in
  let client = Hub.add_client (ic, oc) db_pool in
  Hub.handle_client client
;;

let initialize_db pool =
  let* schema_creation_result = Db.Repository.create_schema pool in
  match schema_creation_result with
  | Ok () -> Log_lwt.info (fun m -> m "Database schema ensured")
  | Error err_msg ->
    let* () = Log_lwt.err (fun m -> m "FATAL: Failed to create database schema: %s" err_msg) in
    Lwt.fail_with "Database initialization failed"
;;

let start port db_pool =
  let* () = initialize_db db_pool in
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let connection_handler_with_db client_addr (ic, oc) =
    connection_handler client_addr (ic, oc) db_pool
  in
  let* _ = Lwt_io.establish_server_with_client_address sockaddr connection_handler_with_db in
  let* () = Log_lwt.info (fun m -> m "Server started on port %d" port) in
  fst @@ Lwt.wait ()
;;
