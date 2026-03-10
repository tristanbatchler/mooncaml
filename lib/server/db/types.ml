type config =
  { host : string
  ; port : int
  ; database : string
  ; user : string
  ; password : string
  ; pool_size : int
  }

type pool =
  { connections : (module Caqti_lwt.CONNECTION) Queue.t
  ; mutex : Lwt_mutex.t
  ; max_size : int
  ; uri : Uri.t
  }
