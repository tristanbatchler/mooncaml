open Lwt.Syntax

(* db.util.ml *)
let build_uri (c : Types.config) =
  let uri =
    Uri.make
      ~scheme:"postgresql"
      ~userinfo:(c.user ^ ":" ^ c.password)
      ~host:c.host
      ~port:c.port
      ~path:c.database
      ()
  in
  Uri.to_string uri
;;

let create_connection uri =
  let* r = Caqti_lwt_unix.connect uri in
  match r with
  | Ok conn -> Lwt.return (Ok conn)
  | Error err -> Lwt.return (Error (`Connection_error (Caqti_error.show err)))
;;

let create_pool config =
  let uri = Uri.of_string (build_uri config) in
  let pool =
    Types.
      { connections = Queue.create ()
      ; mutex = Lwt_mutex.create ()
      ; max_size = config.pool_size
      ; uri
      }
  in
  (* Warm one connection *)
  let* first = create_connection uri in
  match first with
  | Ok conn ->
    Queue.add conn pool.connections;
    Lwt.return (Ok pool)
  | Error e -> Lwt.return (Error e)
;;

let get_connection (pool : Types.pool) =
  let* () = Lwt_mutex.lock pool.mutex in
  let op =
    if Queue.is_empty pool.connections
    then create_connection pool.uri
    else Lwt.return (Ok (Queue.take pool.connections))
  in
  Lwt_mutex.unlock pool.mutex;
  op
;;

let return_connection (pool : Types.pool) conn =
  let* () = Lwt_mutex.lock pool.mutex in
  if Queue.length pool.connections < pool.max_size then Queue.add conn pool.connections;
  Lwt_mutex.unlock pool.mutex;
  Lwt.return_unit
;;

let with_connection (pool : Types.pool) f =
  let* c_res = get_connection pool in
  match c_res with
  | Error e -> Lwt.return (Error e)
  | Ok conn ->
    Lwt.finalize
      (fun () ->
         let* r = f conn in
         Lwt.return (Ok r))
      (fun () -> return_connection pool conn)
;;

let unwrap_result = function
  | Ok (Ok x) -> Ok x
  | Ok (Error e) ->
    Logs.err (fun m -> m "DB Request Error: %s" (Caqti_error.show e));
    Error "Database query failed"
  | Error _ ->
    Logs.err (fun m -> m "DB Connection Error: Failed to acquire connection from pool");
    Error "Database connection failed"
;;
