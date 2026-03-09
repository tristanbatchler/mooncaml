open Lwt.Syntax
open Mooncaml_shared

let src =
  Logs.Src.create "mooncaml_server.hub" ~doc:"The central hub that orchestrates client interactions"
;;

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)
module IntMap = Map.Make (Int)

(* ── State stuff -───────────────────────────────────────────── *)

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
  let clients = !state.clients in
  Lwt_list.iter_p
    (fun (cid, (client : Client.t)) ->
       if cid <> sender_id then Packet.send client.oc packet else Lwt.return_unit)
    (IntMap.bindings clients)
;;

(* ── Game-logic  ────────────────────────────────────────────── *)

let starting_map = Maps.get Maps.Oasis

let try_move_player client_id x y =
  let client = IntMap.find client_id !state.clients in
  if x < 0 || y < 0 || x >= client.map.width || y >= client.map.height
  then false
  else (
    let terrain = client.map.terrain_map.(y).(x) in
    let can_move =
      match terrain with
      | Grass | Dirt -> true
      | _ -> false
    in
    if not can_move
    then false
    else (
      let st = !state in
      match IntMap.find_opt client_id st.players with
      | None -> false
      | Some player ->
        let player' = Entities.{ player with x; y } in
        modify (fun st -> { st with players = IntMap.add client_id player' st.players });
        true))
;;

let get_all_players () = IntMap.bindings !state.players |> List.map snd
let get_player client_id () = IntMap.find client_id !state.players

(* ── Client lifecycle ────────────────────────────────────────── *)

let add_client ic oc =
  let id = !state.next_client_id in
  let rec spawn_point () =
    let x = Random.int starting_map.width in
    let y = Random.int starting_map.height in
    let terrain = starting_map.terrain_map.(y).(x) in
    match terrain with
    | Grass | Dirt -> x, y
    | _ ->
      Log.debug (fun m -> m "Spawn point at (%d, %d) was not suitable for player, retrying..." x y);
      spawn_point ()
  in
  let x, y = spawn_point () in
  let player = Entities.{ id; name = Printf.sprintf "Player %d" id; x; y } in
  let client : Client.t =
    { id
    ; broadcast
    ; ic
    ; oc
    ; try_move_player = try_move_player id
    ; get_all_players
    ; get_player = get_player id
    ; map = starting_map
    }
  in
  modify (fun st ->
    { clients = IntMap.add id client st.clients
    ; players = IntMap.add id player st.players
    ; map = st.map
    ; next_client_id = st.next_client_id + 1
    });
  client
;;

let remove_client client_id =
  modify (fun st ->
    { st with
      clients = IntMap.remove client_id st.clients
    ; players = IntMap.remove client_id st.players
    })
;;

(* ── Per-client read loop ────────────────────────────────────── *)

let rec client_loop (client : Client.t) =
  let* line_opt = Lwt_io.read_line_opt client.ic in
  match line_opt with
  | None -> Lwt.return_unit
  | Some line ->
    let* () =
      match Packet.packet_of_string line with
      | Ok packet -> Client.handle_packet packet client.id client
      | Error err ->
        let* () =
          Log_lwt.err (fun m -> m "Error parsing packet from client %d: %s" client.id err)
        in
        let response = Packet.UnexpectedServerError ("Failed to parse packet: " ^ err) in
        Packet.send client.oc response
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
       let* () = broadcast (Packet.DisconnectEvent { sender_id = client.id }) client.id in
       remove_client client.id;
       let* () = Lwt_io.close client.ic in
       Lwt_io.close client.oc)
;;
