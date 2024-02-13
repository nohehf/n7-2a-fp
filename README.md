# n7 functionnal programming

## Setup ocaml

Create env:

```bash
opam switch create . 4.12.2
brew install --cask xquartz
opam install ocamlformat menhir dune utop merlin ppx_inline_test ppx_expect graphics ocaml-lsp-server

eval $(opam env)
```
