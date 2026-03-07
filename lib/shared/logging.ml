let src = Logs.Src.create "mooncaml_shared.logging" ~doc:"The logging module itself"

module Log = (val Logs.src_log src : Logs.LOG)

let tag_client_id : int Logs.Tag.def =
  Logs.Tag.def "client_id" ~doc:"The ID of the connected client" Format.pp_print_int
;;

let tag_with_client id = Logs.Tag.(empty |> add tag_client_id id)

let custom_reporter () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let src_name = Logs.Src.name src in
    let print_log header tags k ppf fmt =
      let client_prefix =
        match tags with
        | None -> ""
        | Some t ->
          (match Logs.Tag.find tag_client_id t with
           | None -> ""
           | Some id -> Printf.sprintf "[Client %d] " id)
      in
      Format.kfprintf
        k
        ppf
        ("%a %s: %s@[" ^^ fmt ^^ "@]@.")
        Logs_fmt.pp_header
        (level, header)
        src_name
        client_prefix
    in
    msgf @@ fun ?header ?tags fmt -> print_log header tags k Format.std_formatter fmt
  in
  { Logs.report }
;;
