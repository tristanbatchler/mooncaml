open Mooncaml

let () = Lwt_main.run @@ Server.start 43216
