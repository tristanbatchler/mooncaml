type t =
  { id : int
  ; ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel
  }
