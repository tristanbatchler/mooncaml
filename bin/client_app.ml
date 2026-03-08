open Mooncaml_client
open Mooncaml_shared

let setup_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logging.custom_reporter ())
;;

let run_netclient () =
  setup_logging ();
  Logs.info (fun m -> m "Booting up...");
  let host = "127.0.0.1"
  and port = 43216 in
  Lwt_main.run @@ Net_client.start host port
;;

let run_game () =
  setup_logging ();
  Logs.info (fun m -> m "Booting up...");
  Game.run ()
;;

let () =
  let usage = Printf.sprintf "Usage: %s [game|netclient]" Sys.argv.(0) in
  let args = Sys.argv in
  if Array.length args <> 2
  then (
    Printf.eprintf "%s\n" usage;
    exit 1);
  match args.(1) with
  | "game" -> run_game ()
  | "netclient" -> run_netclient ()
  | _ ->
    Printf.eprintf "%s\n" usage;
    exit 1
;;
