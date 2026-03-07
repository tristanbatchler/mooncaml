open Lwt.Syntax

type t =
  { id : int
  ; broadcast : Packet.t -> int -> unit Lwt.t
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  }

let handle_say (packet : Packet.t) sender_id client =
  match packet with
  | Packet.Say msg ->
    if sender_id = client.id
    then client.broadcast packet client.id
    else Lwt_io.write_line client.oc (Printf.sprintf "Client %d says: %s" sender_id msg)
  | _ -> raise (Invalid_argument "Received non-say packet in handle_say")
;;

let handle_move (packet : Packet.t) sender_id client =
  match packet with
  | Packet.Move (x, y) ->
    if sender_id = client.id
    then client.broadcast packet client.id
    else Lwt_io.write_line client.oc (Printf.sprintf "Client %d moved to (%d, %d)" sender_id x y)
  | _ -> raise (Invalid_argument "Received non-move packet in handle_move")
;;

let handle_packet packet sender_id client =
  let* () = Lwt_io.printlf "Handling packet: %s" (Packet.string_of_packet packet) in
  match packet with
  | Packet.Say _ -> handle_say packet sender_id client
  | Packet.Move _ -> handle_move packet sender_id client
;;
