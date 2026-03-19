let build_uri (c : Types.config) =
  let uri =
    Uri.make
      ~scheme:"postgresql"
      ~userinfo:(c.user ^ ":" ^ c.password)
      ~host:c.host
      ~port:c.port
      ~path:c.database
      ()
  in
  Uri.to_string uri
;;

let create_pool config =
  let uri = Uri.of_string (build_uri config) in
  let pool_config = Caqti_pool_config.create ~max_size:config.pool_size () in
  match Caqti_lwt_unix.connect_pool ~pool_config uri with
  | Ok pool -> Ok pool
  | Error err_string ->
    (* Caqti's connect_pool returns a string error if the URI is bad *)
    Error (`Connection_error err_string)
;;
