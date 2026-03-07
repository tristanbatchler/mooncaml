# Project Summary


## .vscode/launch.json

```json
{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        {
            "name": "OCaml earlybird (experimental)",
            "type": "ocaml.earlybird",
            "request": "launch",
            "program": "${workspaceFolder}/_build/default/bin/main.bc",
            "console": "integratedTerminal",
            "stopOnEntry": false,
            "yieldSteps": 1024,
            "cwd": "${workspaceFolder}"
        }
    ]
}
```

## .vscode/settings.json

```json
{
    "[ocaml]": {
        "editor.formatOnSave": true
    }
}
```

## LICENSE

```text
Copyright 2024 Tristan Batchler

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

```

## README.md

```
To obtain:

```shell
git clone https://github.com/tristanbatchler/mooncaml
cd mooncaml
opam switch create . --no-install --no-switch
opam install --deps-only .
eval $(opam env --switch=.)
```

To compile and run: 

```shell
dune exec mooncaml
```

To contribute:

```shell
opam install ocamlformat earlybird ocaml-lsp-server
```
```

## bin/dune

```
(executable
 (public_name mooncaml)
 (name main)
 (libraries mooncaml lwt lwt.unix sexplib)
 (modes exe byte))

```

## bin/main.ml

```ocaml
open Lwt.Syntax
open Mooncaml

(* open Sexplib *)
module IntMap = Map.Make (Int)

type client = {
  id : int;
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
}

let clients = ref IntMap.empty
let next_client_id = ref 0
let num_connected_clients = ref 0

let add_client ic oc =
  let client = { id = !next_client_id; ic; oc } in
  clients := IntMap.add client.id client !clients;
  incr next_client_id;
  incr num_connected_clients;
  let* () =
    Lwt_io.printlf "Added client with ID %d: currently %d clients connected\n"
      client.id !num_connected_clients
  in
  Lwt.return client

let remove_client client =
  clients := IntMap.remove client.id !clients;
  decr num_connected_clients;
  let* () =
    Lwt_io.printlf "Removed client with ID %d: currently %d clients connected\n"
      client.id !num_connected_clients
  in
  Lwt.return ()

let handle_say_packet msg client =
  let* () = Lwt_io.printlf "Client %d says: %s" client.id msg in
  let* () = Lwt_io.write_line client.oc "Congrats on saying something!" in
  Lwt.return ()

let handle_move_packet (x, y) client =
  let* () = Lwt_io.printlf "Client %d moves to (%d, %d)" client.id x y in
  let* () = Lwt_io.write_line client.oc "Congrats on moving!" in
  Lwt.return ()

let handle_packet packet client =
  let* () =
    Lwt_io.printlf "Handling packet: %s" (Packet.string_of_packet packet)
  in
  match packet with
  | Packet.Say msg -> handle_say_packet msg client
  | Packet.Move (x, y) -> handle_move_packet (x, y) client
  | exception exn ->
      let* () =
        Lwt_io.printlf "Error handling packet: %s" (Printexc.to_string exn)
      in
      Lwt.return ()

let rec client_loop client =
  let* line_opt = Lwt_io.read_line_opt client.ic in
  match line_opt with
  | Some line ->
      let* () =
        match Packet.packet_of_string line with
        | Ok packet -> handle_packet packet client
        | Error err ->
            Lwt_io.eprintlf "Error parsing packet from client %d: %s" client.id
              err
      in
      client_loop client
  | None -> Lwt.return_unit

let handle_client client =
  Lwt.finalize
    (fun () -> client_loop client)
    (fun () ->
      let* () = Lwt_io.printlf "Cleaning up client %d" client.id in
      let* () = remove_client client in
      let* () = Lwt_io.close client.ic in
      Lwt_io.close client.oc)

let connection_handler client_addr (ic, oc) =
  let* () =
    match client_addr with
    | Unix.ADDR_INET (inet_addr, port) ->
        Lwt_io.printlf "Accepted connection from %s:%d"
          (Unix.string_of_inet_addr inet_addr)
          port
    | Unix.ADDR_UNIX path ->
        Lwt_io.printlf "Accepted connection from UNIX socket: %s" path
  in
  let* client = add_client ic oc in
  handle_client client

let main () =
  let port = 43216 in
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let* _ =
    Lwt_io.establish_server_with_client_address sockaddr connection_handler
  in
  let* () = Lwt_io.printlf "Server listening on port %d" port in
  let t, _ = Lwt.wait () in
  t

let () = Lwt_main.run (main ())

```

## dune-project

```
(lang dune 3.7)

(name mooncaml)

(generate_opam_files true)

(source
 (github tristanbatchler/mooncaml))

(authors "Tristan Batchler <info@tbat.me>")

(maintainers "Tristan Batchler <info@tbat.me>")

(license MIT)

(documentation
 https://github.com/tristanbatchler/mooncaml/blob/main/README.md)

(package
 (name mooncaml)
 (synopsis "A Moonlapse MUD server implementation in OCaml")
 (description
  "Moonlapse MUD is a struggling-to-exist game. This is a failed attempt (calling it early) to bring it to existence with the power of functional programming. No, it's really just a fun way to spend my weekend. Oh well ¯\\_(ツ)_/¯")
 (depends ocaml dune)
 (tags
  ("moonlapse" "mud" "game" "server")))

(map_workspace_root false)

```

## lib/dune

```
(library
 (name mooncaml)
 (libraries sexplib)
 (preprocess
  (pps ppx_sexp_conv)))

```

## test/dune

```
(test
 (name test_mooncaml)
 (libraries mooncaml))

```

## test/test_mooncaml.ml

```ocaml

```
