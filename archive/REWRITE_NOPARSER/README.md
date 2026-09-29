# REWRITE_NOPARSER — quarantined research lab

**Status: NOT part of the primary QAM-OCAML build.** This directory is a
separate, self-contained OCaml package (`qam_interpreter`) with its own
`dune-project`. It is a semantics lab for quantum operations (no-cloning,
entanglement swap, quantum teleportation, superdense coding) that has **no
lexer/parser** and uses an AST that is **incompatible** with the canonical AST
in [`../../qam_ast/ast.ml`](../../qam_ast/ast.ml):

- its `resource` uses `SimpleResource of resource` (recursive, no string base)
  and an extra `Quantum of string` constructor;
- its actions are `Encode`/`Decode` rather than the canonical
  `LeftCombine`/`RightCombine`.

Porting these semantics onto the canonical AST is deferred (audit Phase 5). Do
not add it to the root dune stanzas. Build/test it on its own:

```bash
cd qam_interpreter && dune build && dune test
```

The primary, buildable track is the repository root.
