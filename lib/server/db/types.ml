type config =
  { host : string
  ; port : int
  ; database : string
  ; user : string
  ; password : string
  ; pool_size : int
  }

type pool = (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
