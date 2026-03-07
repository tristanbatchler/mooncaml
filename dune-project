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
