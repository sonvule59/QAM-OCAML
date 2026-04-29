# REWRITE Track

Active lexer/parser/`Menhir`/interpreter prototype for membranes and processes.

### Layout (`dune` splits libraries to avoid duplicate module errors)

| Path | Purpose |
|------|---------|
| `qam_ast/ast.ml` | AST definitions (shared everywhere) |
| `grammar/` | `lexer.mll`, `parser.mly`; library **`grammar`** (Lexer + Parser) |
| *top level* | `checkEquivalence.ml`, `interpreter.ml`; library **`qam_rewrite`** |
| `main.ml` | REPL entry (`dune exec -- main`) |

Menhir/ocamllex live only under `grammar/`, so they are not listed twice across an executable and a test stanza.

## Build and run (from this directory)

Requirements: **`opam`**, plus packages `dune`, `menhir`, and `ounit2`.

### Toolchain install (once)

**macOS with [Homebrew](https://brew.sh):**

```bash
brew install opam
opam init
eval "$(opam env)"    # put this line in ~/.zshrc too so new terminals see opam tools
opam install dune menhir ounit2
```

(On Apple Silicon, Homebrew puts binaries under `/opt/homebrew/bin`; ensure that directory is on your `PATH`.)

**If you see `zsh: command not found: opam`:** install Opam first—`opam install …` only works **after** `brew install opam` (or another install method). The [official install docs](https://opam.ocaml.org/doc/Install.html) cover Linux and other setups.

### Build

```bash
dune build
dune test
dune exec -- main                 # interactive REPL (executable entry module is main.ml)
dune exec -- chem_main -- --benchmark
dune exec -- chem_main -- --dsl examples/chem/h2.qamchem
```

Or:

```bash
./run.sh
```

## Grammar (surface syntax)

Input is one or more membranes separated by commas.

- Inner membrane `{ ... }`: comma-separated molecules (process prefixes or resources).
- **Choice**: `p + q` (left-associative).
- **Replication**: `repl p` (`repl` binds tighter than `+`).
- **Airlock membrane**: **`|[`** *left* **`,`** *resource* **`,`** *right* **`]|`** (close is `]` immediately followed by `|`).

Primitives match the existing action forms: `nu c.`, `a!x.`, `b?y.`, `i<-j.`, `i->k.`, and resources `o` or identifiers.

## Pipeline

1. `grammar/lexer.mll` → tokens (including `+`, `repl`, `|[`, `]|`).
2. `grammar/parser.mly` (`Menhir`) → `Ast.membrane list`.
3. `interpreter.ml` applies reduction passes (`ENCODE`, `DECODE`, `COHERE`) and repeats them to approximate a normal form.
4. `checkEquivalence.ml` compares AST shapes; normalization-based “equivalence” uses structural equality of normal forms.

## Quantum chemistry track (minimal VQE)

This repo now includes a lightweight chemistry interpreter path:

- `chemistry.ml`: tiny chemistry DSL parser + toy VQE optimizer + OpenQASM export.
- `chem_main.ml`: CLI entry for chemistry runs and benchmark reports.
- `examples/chem/h2.qamchem` and `examples/chem/lih.qamchem`: benchmark inputs.

### Chemistry DSL

Each line is one record:

```text
name H2
qubits 2
ref -1.137
term -1.0523732 I0
term 0.3979374 Z0
term -0.3979374 Z1
term -0.0112801 Z0 Z1
term 0.1809312 X0 X1
```

### What this gives you today

1. Chemistry DSL subset parser (`name`, `qubits`, `ref`, `term`).
2. End-to-end VQE loop (finite-difference gradient descent).
3. OpenQASM 2.0 circuit export for the variational ansatz.
4. Built-in benchmark suite for `H2` and `LiH`.
5. Convergence reporting (`final energy`, `reference`, `delta`, iterations).

## Examples

Choice and replication parse as AST nodes (`Choice`, `Replication`):

```text
{ a!x. + b?y., o }
```

```text
{ repl nu c., o }
```

Airlock parses to `Ast.AirlockedMembrane` when you write nested membranes explicitly:

```text
|[ { nu z., o }, phi, { c?d. }]|
```

## Caveats

Full bisimulation semantics are **not** implemented; “equivalence” here means **equal normal forms after the built-in reductions** on this AST subset.
