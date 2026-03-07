open Lwt.Syntax

let handle_packet packet (client : State.client) =
  let* () = Lwt_io.printlf "Handling packet: %s" (Packet.string_of_packet packet) in
  match packet with
  | Packet.Say msg ->
    let* () = Lwt_io.printlf "Client %d says: %s" client.id msg in
    let* () = Lwt_io.write_line client.oc "Congrats on saying something!" in
    Lwt.return ()
  | Packet.Move (x, y) ->
    let* () = Lwt_io.printlf "Client %d moves to (%d, %d)" client.id x y in
    let* () = Lwt_io.write_line client.oc "Congrats on moving!" in
    Lwt.return ()
  | exception exn ->
    let* () =
      Lwt_io.printlf "Error handling packet from client %d: %s" client.id (Printexc.to_string exn)
    in
    Lwt.return ()
;;

let rec client_loop (client : State.client) =
  let* line_opt = Lwt_io.read_line_opt client.ic in
  match line_opt with
  | None -> Lwt.return_unit
  | Some line ->
    let* () =
      match Packet.packet_of_string line with
      | Ok packet -> client |> handle_packet packet
      | Error err -> Lwt_io.eprintlf "Error parsing packet from client %d: %s" client.id err
    in
    client_loop client
;;

let handle_client (client : State.client) =
  Lwt.finalize
    (fun () -> client_loop client)
    (fun () ->
       let* () = Lwt_io.printlf "Cleaning up client %d" client.id in
       State.remove_client client.id;
       let* () = Lwt_io.close client.ic in
       Lwt_io.close client.oc)
;;

let _connection_handler client_addr (ic, oc) =
  let* () =
    match client_addr with
    | Unix.ADDR_INET (inet_addr, port) ->
      Lwt_io.printlf "Accepted connection from %s:%d" (Unix.string_of_inet_addr inet_addr) port
    | _ -> Lwt_io.eprintlf "Rejected connection from unsupported address type"
  in
  let client = State.add_client ic oc in
  handle_client client
;;

let start port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let* _ = Lwt_io.establish_server_with_client_address sockaddr _connection_handler in
  let* () = Lwt_io.printlf "Server started on port %d" port in
  let forever, _ = Lwt.wait () in
  forever
;;
