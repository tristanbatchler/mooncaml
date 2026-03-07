open Mooncaml_server
open Mooncaml_shared

let setup_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logging.custom_reporter ())
;;

let () =
  setup_logging ();
  Logs.info (fun m -> m "Booting up...");
  Lwt_main.run @@ Server.start 43216
;;
