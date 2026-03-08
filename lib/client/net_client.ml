open Lwt.Syntax

let src = Logs.Src.create "mooncaml_client.net_client" ~doc:"Low-level client operations"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let rec listen ic =
  let* line_opt = Lwt_io.read_line_opt ic in
  match line_opt with
  | Some line ->
    let _ =
      match Mooncaml_shared.Packet.packet_of_string line with
      | Ok p -> Game.server_outstream (Some p)
      | Error _ -> ()
    in
    listen ic
  | None -> Lwt.return (Game.server_outstream None)
;;

let start host port =
  let* addr_info = Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ] in
  let addr = (List.hd addr_info).Unix.ai_addr in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket addr in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
  let* () = Log_lwt.info (fun m -> m "Connected to %s:%d" host port) in
  Lwt.join [ listen ic; Game.run ic oc () ]
;;
