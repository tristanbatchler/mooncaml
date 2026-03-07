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
dune exec server_app
# or
dune exec client_app
```

To contribute:

```shell
opam install ocamlformat earlybird ocaml-lsp-server
```