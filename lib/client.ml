open Lwt.Syntax

type t =
  { id : int
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  }

let handle_say msg client =
  let* () = Lwt_io.printlf "Client %d says: %s" client.id msg in
  let* () = Lwt_io.write_line client.oc "Congrats on saying something!" in
  Lwt.return ()
;;

let handle_move (x, y) client =
  let* () = Lwt_io.printlf "Client %d moves to (%d, %d)" client.id x y in
  let* () = Lwt_io.write_line client.oc "Congrats on moving!" in
  Lwt.return ()
;;

let handle_packet packet client =
  let* () = Lwt_io.printlf "Handling packet: %s" (Packet.string_of_packet packet) in
  match packet with
  | Packet.Say msg -> handle_say msg client
  | Packet.Move (x, y) -> handle_move (x, y) client
;;
