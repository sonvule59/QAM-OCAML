# QAM-OCAML
This is my project for Quantum Abstract Machines implementation in OCAML
@Author: Son Vu
@Date: Spring 2024

## Repository tracks

- `REWRITE/`: primary track with lexer/parser (`ocamllex` + `menhir`), AST, reduction interpreter, and a normalize-then-canonicalize equivalence checker (see the [Equivalence policy](REWRITE/README.md#equivalence-policy)).
- `REWRITE_NOPARSER/`: parallel semantics lab without a parser; useful for exploring standalone quantum operations.

If you are evaluating "current compiler state," start with `REWRITE/` because it is the track where syntax, parsing, and execution are connected.

## Quick start (`REWRITE`)

**macOS (Homebrew)** — install `opam` first (`opam` is not bundled with the OS):

```bash
brew install opam
opam init
eval "$(opam env)"
opam install dune menhir ounit2
```

Add `eval "$(opam env)"` to `~/.zshrc` (or run it in each new terminal). Then:

```bash
cd REWRITE
dune build && dune test
dune exec -- main
```

**Linux / elsewhere:** see [Install OCaml](https://opam.ocaml.org/doc/Install.html) (`opam` from your distro or the official installer), then run `opam install dune menhir ounit2` as above.

**Why `command not found: opam`?** Nothing is wrong with your shell—you still need to install Opam (`brew install opam` on macOS) before `opam` exists on your `PATH`.

Details, grammar, and examples are in [REWRITE/README.md](REWRITE/README.md).
