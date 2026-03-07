module IntMap = Map.Make (Int)

type client =
  { id : int
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  }

type t =
  { clients : client IntMap.t
  ; next_client_id : int
  ; num_connected_clients : int
  }

let current_state = ref { clients = IntMap.empty; next_client_id = 0; num_connected_clients = 0 }

let add_client ic oc =
  let client = { id = !current_state.next_client_id; ic; oc } in
  let new_clients = IntMap.add client.id client !current_state.clients in
  current_state
  := { clients = new_clients
     ; next_client_id = !current_state.next_client_id + 1
     ; num_connected_clients = !current_state.num_connected_clients + 1
     };
  client
;;

let remove_client client_id =
  current_state
  := { !current_state with
       clients = IntMap.remove client_id !current_state.clients
     ; num_connected_clients = !current_state.num_connected_clients - 1
     }
;;
