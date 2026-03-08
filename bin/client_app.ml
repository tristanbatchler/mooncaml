open Mooncaml_client
open Mooncaml_shared

let setup_logging () =
  let oc = open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 "client.log" in
  let destination = Format.formatter_of_out_channel oc in
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logging.custom_reporter ~destination ())
;;

let () =
  setup_logging ();
  Logs.info (fun m -> m "Booting up...");
  let host = "127.0.0.1"
  and port = 43216 in
  Lwt_main.run @@ Net_client.start host port
;;
