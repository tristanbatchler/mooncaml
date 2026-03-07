open Lwt.Syntax

let connection_handler client_addr (ic, oc) =
  let* () =
    match client_addr with
    | Unix.ADDR_INET (inet_addr, port) ->
      Lwt_io.printlf "Accepted connection from %s:%d" (Unix.string_of_inet_addr inet_addr) port
    | _ -> Lwt_io.eprintlf "Rejected connection from unsupported address type"
  in
  let client = Hub.add_client ic oc in
  let* () =
    Lwt_io.printlf
      "Client %d connected: currently %d clients connected"
      client.id
      !Hub.state.num_connected_clients
  in
  Hub.handle_client client
;;

let start port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let* _ = Lwt_io.establish_server_with_client_address sockaddr connection_handler in
  let* () = Lwt_io.printlf "Server started on port %d" port in
  let forever, _ = Lwt.wait () in
  forever
;;
