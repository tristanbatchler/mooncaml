open Mooncaml_client
open Mooncaml_shared

let default_host = "127.0.0.1"
and default_port = 43216

let setup_logging () =
  let oc = open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 "client.log" in
  let destination = Format.formatter_of_out_channel oc in
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logging.custom_reporter ~destination ())
;;

let () =
  let args = Sys.argv in
  let host, port =
    match args with
    | [| _; host; port_str |] -> host, int_of_string port_str
    | [| _; host |] -> host, default_port
    | _ -> default_host, default_port
  in
  setup_logging ();
  Logs.info (fun m -> m "Booting up...");
  Lwt_main.run @@ Net_client.start host port
;;
