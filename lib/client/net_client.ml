open Lwt.Syntax

let src = Logs.Src.create "mooncaml_client.net_client" ~doc:"Low-level client operations"

module Log = (val Logs.src_log src : Logs.LOG)
module Log_lwt = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let start host port =
  let* addr_info = Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ] in
  let* addr =
    match addr_info with
    | [] -> Lwt.fail_with "Failed to resolve host"
    | addr :: _ -> Lwt.return addr.Unix.ai_addr
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket addr in
  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  (* Loop 1: Only cares about the Keyboard -> Server pipeline *)
  let rec read_keyboard_and_send () =
    let* line_opt = Lwt_io.read_line_opt Lwt_io.stdin in
    match line_opt with
    | Some message ->
      let* () = Lwt_io.write_line output message in
      read_keyboard_and_send ()
    | None ->
      (* User hit Ctrl+D *)
      Lwt.return_unit
  in
  (* Loop 2: Only cares about the Server -> Screen pipeline *)
  let rec read_server_and_print () =
    let* response_opt = Lwt_io.read_line_opt input in
    match response_opt with
    | Some response ->
      let* () = Log_lwt.info (fun m -> m "Server: %s" response) in
      read_server_and_print ()
    | None ->
      let* () = Log_lwt.info (fun m -> m "Disconnected from server.") in
      Lwt.return_unit
  in
  let* () = Log_lwt.info (fun m -> m "Connected to %s:%d" host port) in
  (* Lwt.pick runs a list of promises. If ANY of them finish (like the server 
     disconnecting or the user hitting Ctrl+D), it cancels the others and returns. *)
  Lwt.pick [ read_keyboard_and_send (); read_server_and_print () ]
;;
