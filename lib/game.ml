open Lwt.Syntax

let handle_say msg (client : Client.t) =
  let* () = Lwt_io.printlf "Client %d says: %s" client.id msg in
  let* () = Lwt_io.write_line client.oc "Congrats on saying something!" in
  Lwt.return ()
;;

let handle_move (x, y) (client : Client.t) =
  let* () = Lwt_io.printlf "Client %d moves to (%d, %d)" client.id x y in
  let* () = Lwt_io.write_line client.oc "Congrats on moving!" in
  Lwt.return ()
;;
