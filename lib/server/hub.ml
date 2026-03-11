open Mooncaml_shared

let src =
  Logs.Src.create "mooncaml_server.hub" ~doc:"The central hub that orchestrates client interactions"
;;

module Log = (val Logs.src_log src : Logs.LOG)
module IntMap = Map.Make (Int)

type t =
  { clients : Client.t IntMap.t
  ; players : Entities.player IntMap.t
  ; map : Maps.map_data
  ; next_client_id : int
  }

let state =
  ref
    { clients = IntMap.empty
    ; players = IntMap.empty
    ; map = Maps.get Maps.Oasis
    ; next_client_id = 0
    }
;;

let modify f =
  state := f !state;
  Log.debug (fun m -> m "Currently %d clients connected" (IntMap.cardinal !state.clients))
;;

let broadcast packet sender_id =
  let current_clients = !state.clients in
  Lwt_list.iter_p
    (fun (cid, (client : Client.t)) ->
       if cid <> sender_id then Packet.send client.oc packet else Lwt.return_unit)
    (IntMap.bindings current_clients)
;;

let try_move_player client_id x y =
  let st = !state in
  if x < 0 || y < 0 || x >= st.map.width || y >= st.map.height
  then false
  else (
    match st.map.terrain_map.(y).(x) with
    | Grass | Dirt ->
      (match IntMap.find_opt client_id st.players with
       | None -> false
       | Some player ->
         let player' = Entities.{ player with x; y } in
         modify (fun s -> { s with players = IntMap.add client_id player' s.players });
         true)
    | _ -> false)
;;

let get_all_players () = IntMap.bindings !state.players |> List.map snd

let remove_player_from_world client_id =
  modify (fun st -> { st with players = IntMap.remove client_id st.players })
;;

(* --- RESTORED: Hub owns the spawning logic! --- *)
let spawn_player client_id username =
  let st = !state in
  let rec spawn_point () =
    let x = Random.int st.map.width in
    let y = Random.int st.map.height in
    match st.map.terrain_map.(y).(x) with
    | Grass | Dirt -> x, y
    | _ -> spawn_point ()
  in
  let x, y = spawn_point () in
  let player = Entities.{ id = client_id; name = username; x; y } in
  modify (fun s -> { s with players = IntMap.add client_id player s.players });
  player
;;

let add_client (ic, oc) db_pool =
  let id = !state.next_client_id in
  let client : Client.t =
    { id
    ; ic
    ; oc
    ; db_pool
    ; map = !state.map
    ; broadcast
    ; try_move_player = try_move_player id
    ; get_all_players
    ; spawn_player = spawn_player id
    ; remove_player_from_world
    }
  in
  modify (fun st ->
    { st with clients = IntMap.add id client st.clients; next_client_id = st.next_client_id + 1 });
  client
;;

let handle_client (client : Client.t) =
  Lwt.finalize
    (fun () -> Client.start client)
    (fun () ->
       modify (fun st -> { st with clients = IntMap.remove client.id st.clients });
       Lwt.return_unit)
;;
