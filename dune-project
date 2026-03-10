(lang dune 3.7)

(name mooncaml)

(generate_opam_files true)

(source
 (github tristanbatchler/mooncaml))

(authors "Tristan Batchler <info@tbat.me>")

(maintainers "Tristan Batchler <info@tbat.me>")

(license MIT)

(package
 (name mooncaml_shared)
 (synopsis "Shared logic for Moonlapse MUD")
 (depends
  ocaml
  dune
  lwt
  logs
  fmt
  sexplib
  ppx_sexp_conv
  ;; Dev Tools attached to the core package
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)
  (earlybird :with-dev-setup)
  (utop :with-dev-setup)))

(package
 (name mooncaml_server)
 (synopsis "A Moonlapse MUD server implementation")
 (depends
  mooncaml_shared
  imagelib
  caqti
  caqti-lwt
  caqti-driver-postgresql
  argon2))

(package
 (name mooncaml_client)
 (synopsis "A terminal client for Moonlapse MUD")
 (depends mooncaml_shared curses))

(map_workspace_root false)
