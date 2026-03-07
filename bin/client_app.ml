open Mooncaml_client

let () =
  print_endline "Hello?";
  let host = "127.0.0.1"
  and port = 43216 in
  Lwt_main.run @@ Net_client.start host port
;;
