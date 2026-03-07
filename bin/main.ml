open Lwt.Syntax
module IntMap = Map.Make (Int)

type client = {
  id : int;
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
}

let clients = ref IntMap.empty
let next_client_id = ref 0
let num_connected_clients = ref 0

let add_client ic oc =
  let client = { id = !next_client_id; ic; oc } in
  clients := IntMap.add client.id client !clients;
  incr next_client_id;
  incr num_connected_clients;
  let* () =
    Lwt_io.printlf "Added client with ID %d: currently %d clients connected\n"
      client.id !num_connected_clients
  in
  Lwt.return client

let remove_client client =
  clients := IntMap.remove client.id !clients;
  decr num_connected_clients;
  let* () =
    Lwt_io.printlf "Removed client with ID %d: currently %d clients connected\n"
      client.id !num_connected_clients
  in
  Lwt.return ()

let handle_say_packet _args client =
  let* () = Lwt_io.printlf "Client %d says something" client.id in
  let* () = Lwt_io.write_line client.oc "Congrats on saying something!" in
  Lwt.return ()

let handle_move_packet _args client =
  let* () = Lwt_io.printlf "Client %d moves somewhere" client.id in
  let* () = Lwt_io.write_line client.oc "Congrats on moving!" in
  Lwt.return ()

let handle_packet packet client =
  let* () = Lwt_io.printlf "Handling packet: %s" packet in
  let parts =
    packet |> String.lowercase_ascii |> String.trim |> String.split_on_char ' '
  in
  match parts with
  | "say" :: args -> handle_say_packet args client
  | "move" :: args -> handle_move_packet args client
  | _ -> Lwt_io.printl "Unknown packet type"

let rec client_loop client =
  let* line_opt = Lwt_io.read_line_opt client.ic in
  match line_opt with
  | Some line ->
      let* () = handle_packet line client in
      client_loop client
  | None -> Lwt.return_unit

let handle_client client =
  Lwt.finalize
    (fun () -> client_loop client)
    (fun () ->
      let* () = Lwt_io.printlf "Cleaning up client %d" client.id in
      let* () = remove_client client in
      let* () = Lwt_io.close client.ic in
      Lwt_io.close client.oc)

let connection_handler client_addr (ic, oc) =
  let* () =
    match client_addr with
    | Unix.ADDR_INET (inet_addr, port) ->
        Lwt_io.printlf "Accepted connection from %s:%d"
          (Unix.string_of_inet_addr inet_addr)
          port
    | Unix.ADDR_UNIX path ->
        Lwt_io.printlf "Accepted connection from UNIX socket: %s" path
  in
  let* client = add_client ic oc in
  handle_client client

let main () =
  let port = 43216 in
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let* _ =
    Lwt_io.establish_server_with_client_address sockaddr connection_handler
  in
  let* () = Lwt_io.printlf "Server listening on port %d" port in
  let t, _ = Lwt.wait () in
  t

let () = Lwt_main.run (main ())
