open Caqti_request.Infix

let query_psql_version = (Caqti_type.unit ->! Caqti_type.string) @@ "SELECT version()"

let get_psql_version pool =
  Util.with_connection pool (fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find query_psql_version ())
;;
