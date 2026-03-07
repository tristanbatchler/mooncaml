open Lwt.Syntax
module IntMap = Map.Make (Int)

type t =
  { clients : Client.t IntMap.t
  ; next_client_id : int
  ; num_connected_clients : int
  }

let state = ref { clients = IntMap.empty; next_client_id = 0; num_connected_clients = 0 }
let modify f = state := f !state

let add_client ic oc =
  let client : Client.t = { id = !state.next_client_id; ic; oc } in
  modify (fun st ->
    let new_clients = IntMap.add client.id client st.clients in
    { clients = new_clients
    ; next_client_id = st.next_client_id + 1
    ; num_connected_clients = st.num_connected_clients + 1
    });
  client
;;

let remove_client client_id =
  modify (fun st ->
    { st with
      clients = IntMap.remove client_id st.clients
    ; num_connected_clients = st.num_connected_clients - 1
    })
;;

let handle_packet packet (client : Client.t) =
  let* () = Lwt_io.printlf "Handling packet: %s" (Packet.string_of_packet packet) in
  match packet with
  | Packet.Say msg -> Game.handle_say msg client
  | Packet.Move (x, y) -> Game.handle_move (x, y) client
;;

let rec client_loop (client : Client.t) =
  let* line_opt = Lwt_io.read_line_opt client.ic in
  match line_opt with
  | None -> Lwt.return_unit
  | Some line ->
    let* () =
      match Packet.packet_of_string line with
      | Ok packet -> handle_packet packet client
      | Error err -> Lwt_io.eprintlf "Error parsing packet from client %d: %s" client.id err
    in
    client_loop client
;;

let handle_client (client : Client.t) =
  Lwt.finalize
    (fun () -> client_loop client)
    (fun () ->
       let* () = Lwt_io.printlf "Cleaning up client %d" client.id in
       remove_client client.id;
       let* () = Lwt_io.printlf "Currently %d clients connected" !state.num_connected_clients in
       let* () = Lwt_io.close client.ic in
       Lwt_io.close client.oc)
;;

let connection_handler client_addr (ic, oc) =
  let* () =
    match client_addr with
    | Unix.ADDR_INET (inet_addr, port) ->
      Lwt_io.printlf "Accepted connection from %s:%d" (Unix.string_of_inet_addr inet_addr) port
    | _ -> Lwt_io.eprintlf "Rejected connection from unsupported address type"
  in
  let client = add_client ic oc in
  let* () =
    Lwt_io.printlf
      "Client %d connected: currently %d clients connected"
      client.id
      !state.num_connected_clients
  in
  handle_client client
;;

let start port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let* _ = Lwt_io.establish_server_with_client_address sockaddr connection_handler in
  let* () = Lwt_io.printlf "Server started on port %d" port in
  let forever, _ = Lwt.wait () in
  forever
;;
