open Lwt.Syntax

let src = Logs.Src.create "mooncaml.server" ~doc:"Low-level server operations"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let connection_handler client_addr (ic, oc) =
  let* () =
    match client_addr with
    | Unix.ADDR_INET (inet_addr, port) ->
      Log_lwt.info (fun m ->
        m "Accepted connection from %s:%d" (Unix.string_of_inet_addr inet_addr) port)
    | _ -> Log_lwt.err (fun m -> m "Rejected connection from unsupported address type")
  in
  let client = Hub.add_client ic oc in
  Hub.handle_client client
;;

let start port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let* _ = Lwt_io.establish_server_with_client_address sockaddr connection_handler in
  let* () = Log_lwt.info (fun m -> m "Server started on port %d" port) in
  fst @@ Lwt.wait ()
;;
