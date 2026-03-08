open Mooncaml_client
open Mooncaml_shared

let setup_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logging.custom_reporter ())
;;

let () =
  setup_logging ();
  Logs.info (fun m -> m "Booting up...");
  let host = "127.0.0.1"
  and port = 43216 in
  Lwt_main.run @@ Net_client.start host port
;;
