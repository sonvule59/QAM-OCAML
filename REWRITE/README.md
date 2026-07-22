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

Primitives match the existing action forms: `nu c.`, `a!x.`, `b?y.`, `i<-j.`, `i->k.`, the null process `0`, and resources `o`, identifiers, or a **meet** `r & s` (`MeetOperation`, left-associative). The meet/`0` forms exist so a reduced term (e.g. ENCODE's output) round-trips back through the parser.

## Pipeline

1. `grammar/lexer.mll` → tokens (including `+`, `repl`, `|[`, `]|`).
2. `grammar/parser.mly` (`Menhir`) → `Ast.membrane list`.
3. `interpreter.ml` applies reduction passes (`ENCODE`, `DECODE`, `COHERE`, plus Choice and Replication) and repeats them (fuel-bounded) to approximate a normal form. `string_of_membrane` renders any term for the REPL.
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

## Examples: parses vs. reduces

"Parses" = the grammar builds an AST. "Reduces" = `normalize` changes the term
via a reduction rule. The two are **not** the same — some terms parse but are
already normal forms.

**Parse and reduce** (reduction regression is locked by `testCase.ml`; see
[`examples/qam/README.md`](examples/qam/README.md) for the exact normal forms):

| Input | Rule | Normal form |
|-------|------|-------------|
| `{ a<-k., a }` | ENCODE | `{ 0, a & a }` |
| `{ nu c., c!x. }` | COHERE | `{ 0, 0 }` |
| `{ c->x., c?y. }` | DECODE | `{ 0, 0 }` |
| `{ a!x. + b?y., o }` | Choice | `{ a!x., o }` (commits to left branch) |
| `{ repl nu c., o }` | Replication | unfolds `repl P → P \| repl P`, bounded by fuel |

**Parses only** (a valid AST, but no rule fires — it is its own normal form):

```text
|[ { nu z., o }, phi, { c?d. }]|
```

This airlock parses to `Ast.AirlockedMembrane`; the only cross-boundary rule is
airlock DECODE (a `->` on the left meeting a matching `?` on the right), which
this term does not trigger.

## Equivalence policy

Equivalence is a **documented, test-locked policy for the supported reducing
fragment**, not bisimulation.

**Definition.** For membranes `m1`, `m2`:

```
m1 ≡ m2   iff   canon(normalize(m1))  =_struct  canon(normalize(m2))
```

- `normalize` (fuel 128) reduces both sides using ENCODE/DECODE/COHERE/Choice/Replication.
- `canon` (`CheckEquivalence.canonicalize`) rewrites the normal form so that
  irrelevant syntactic differences collapse.
- `=_struct` is the structural walk `CheckEquivalence.check_membrane_equivalence`.
- Both sides use the **same** fuel, so equivalence is not fuel-sensitive.

**What canon normalizes (IN — we claim these):**

- **Soup as multiset** — molecule order in a `MoleculeMembrane` is irrelevant
  (`{ 0, a & a } ≡ { a & a, 0 }`).
- **Meet commutativity** — `a & b ≡ b & a`.
- **Reduce-then-equal** — terms with the same canonical normal form
  (`{ nu c., c!x. } ≡ { 0, 0 }`, `{ a<-k., a } ≡ { 0, a & a }`).
- **NullMolecule cleanup** — dropped (never produced by parse/reduction).

**What it does NOT do (OUT — explicit non-claims):**

- **Bisimulation / observational equivalence** — not implemented (Phase 5).
- **Choice commutativity** — `p + q` is *not* `q + p`. Choice commits to the
  left branch (MVP), so the two normalize to different terms and are reported
  **NotEquivalent**. Locked by `test_not_equiv_choice_asymmetry`.
- **Replication** — terms with live `repl` never reach a true fixpoint; they are
  compared only as fuel-bounded approximations. Identical `repl` terms are
  equivalent (same fuel); a `repl` term is **not** claimed equivalent to its
  one-step unfold.
- **Meet associativity flattening** — not performed; only binary commutativity.
- **Airlocks** — compared structurally (children canonicalized); no new airlock
  equivalence laws.
- **Alpha-equivalence / binder renaming** — not implemented.

See [`examples/qam/equiv.md`](examples/qam/equiv.md) for the worked ≡ / ≢ table.
