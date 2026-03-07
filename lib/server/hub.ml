open Lwt.Syntax
open Mooncaml_shared

let src =
  Logs.Src.create "mooncaml_server.hub" ~doc:"The central hub that orchestrates client interactions"
;;

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)
module IntMap = Map.Make (Int)

type t =
  { clients : Client.t IntMap.t
  ; next_client_id : int
  ; num_connected_clients : int
  }

let state = ref { clients = IntMap.empty; next_client_id = 0; num_connected_clients = 0 }

let modify f =
  state := f !state;
  Log.debug (fun m -> m "Currently %d clients connected" !state.num_connected_clients)
;;

let broadcast packet sender_id =
  let clients = !state.clients in
  Lwt_list.iter_p
    (fun (cid, client) ->
       if cid <> sender_id then Client.handle_packet packet sender_id client else Lwt.return_unit)
    (IntMap.bindings clients)
;;

let add_client ic oc =
  let client : Client.t = { id = !state.next_client_id; broadcast; ic; oc } in
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

let rec client_loop (client : Client.t) =
  let* line_opt = Lwt_io.read_line_opt client.ic in
  match line_opt with
  | None -> Lwt.return_unit
  | Some line ->
    let* () =
      match Packet.packet_of_string line with
      | Ok packet -> Client.handle_packet packet client.id client
      | Error err ->
        Log_lwt.err (fun m -> m "Error parsing packet from client %d: %s" client.id err)
    in
    client_loop client
;;

let handle_client (client : Client.t) =
  Lwt.finalize
    (fun () -> client_loop client)
    (fun () ->
       let* () =
         Log_lwt.info (fun m -> m "Disconnected" ~tags:(Logging.tag_with_client client.id))
       in
       remove_client client.id;
       let* () = Lwt_io.close client.ic in
       Lwt_io.close client.oc)
;;
