# QAM-OCAML

[![CI](https://github.com/sonvule59/QAM-OCAML/actions/workflows/ci.yml/badge.svg)](https://github.com/sonvule59/QAM-OCAML/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

An OCaml implementation of the **Quantum Abstract Machine (QAM)** of
Li, Chang, Cleaveland, Zhu and Wu —
[*The Quantum Abstract Machine*, arXiv:2402.13469](https://arxiv.org/abs/2402.13469) —
a CHAM-style abstract machine for describing and verifying hybrid
classical–quantum network (HCQN) protocols such as quantum teleportation,
without writing circuits by hand.

**What you get:**

- **Parser + reduction interpreter** for QAM membranes, molecules, processes
  and resources (ENCODE / DECODE / COHERE, Choice, Replication).
- **Equivalence checker** — normalize, canonicalize, structurally compare
  (documented policy below; not bisimulation).
- **Compiler to OpenQASM 2.0** — protocols written as QAM configurations lower
  to real circuits via the paper's task-block gate mappings.
- **Built-in statevector simulator** — no Python/Qiskit needed; `dune test`
  *proves* that the compiled teleportation circuit reproduces the message state
  on Bob's qubit in every measurement branch.

## Quick start

Requires [opam](https://opam.ocaml.org/doc/Install.html), then:

```bash
opam install dune menhir ounit2
dune build && dune test          # 32 tests
```

To install the `qam` binary into your opam switch: `opam install .`

## Demo: quantum teleportation in five lines of QAM

`examples/qam/teleport.qam` (the paper's Example 2 — Alice teleports message
`d` to Bob over quantum channel `c`, sending her classical residue on `a`):

```text
{ nu c.c<-d.c->x.a!x., d, o }, { nu c.c?u.a?z.u<-z., o }
```

Compile it to a circuit:

```bash
$ dune exec ./qam.exe -- compile examples/qam/teleport.qam
OPENQASM 2.0;
include "qelib1.inc";
qreg q[3];
creg m0[1];
creg m1[1];
h q[1];
cx q[1], q[2];                  // Bell pair between Alice and Bob
cx q[0], q[1];
h q[0];                         // encode the message onto the channel
measure q[1] -> m0[0];
measure q[0] -> m1[0];          // decode: X and Z correction bits
if(m0==1) x q[2];
if(m1==1) z q[2];               // Bob recovers the message
```

Simulate it (`qam simulate` runs the built-in statevector simulator, branching
on measurements):

```bash
$ dune exec ./qam.exe -- simulate examples/qam/teleport.qam
```

The test suite goes further: `test_teleport_simulates` initializes the message
qubit to `0.6|0⟩ + 0.8|1⟩` and asserts that **every** measurement branch has
probability exactly 1/4 and leaves Bob's qubit in the message state. Also
included: bit-commitment (paper Example 1) and superdense coding (Example 21) —
see [`examples/qam/README.md`](examples/qam/README.md).

## CLI

```text
qam compile FILE.qam    compile a QAM configuration to OpenQASM 2.0
qam simulate FILE.qam   compile, then run on the built-in statevector simulator
qam repl                interactive reduction / equivalence REPL
```

(From a clone, prefix with `dune exec ./qam.exe --`; or `./run.sh <args>`.)

## The pipeline

| Stage | Where |
|-------|-------|
| Lexer / Menhir grammar → `Ast.membrane list` | `grammar/`, `qam_ast/ast.ml` |
| Reduction passes (ENCODE, DECODE, COHERE, Choice, Replication), fuel-bounded `normalize`, term printer | `interpreter.ml` |
| Canonicalizer + structural equivalence | `checkEquivalence.ml` |
| QAM → OpenQASM 2.0 backend | `compile.ml` |
| Statevector simulator for the emitted QASM subset | `sim.ml` |
| CLI (`compile` / `simulate` / `repl`) | `qam.ml` |

## Surface syntax

Input is one or more membranes separated by commas.

- Inner membrane `{ ... }`: comma-separated molecules (process prefixes or resources).
- **Choice**: `p + q` (left-associative).
- **Replication**: `repl p` (`repl` binds tighter than `+`).
- **Airlock membrane**: **`|[`** *left* **`,`** *resource* **`,`** *right* **`]|`**.

Action prefixes: `nu c.` (quantum channel creation, the paper's ν),
`a!x.` (classical send), `b?y.` (receive), `c<-m.` (encode, the paper's ◁),
`c->x.` (decode, ▷); the null process is `0`; resources are the blank `o`
(the paper's ◦), identifiers, or a **meet** `r & s` (⊙, left-associative).

**Action prefixes sequence** (each trailing `.` ends one prefix):
`nu c.c->x.a!x.` is ν c, then decode, then send — the paper's `A R` form.
Prefixing binds tighter than `+`, so `a!x.b?y. + c!z.` is `(a!x.b?y.) + (c!z.)`.
Parse errors report line and column.

## Reductions: parses vs. reduces

"Parses" = the grammar builds an AST. "Reduces" = `normalize` changes the term
via a reduction rule. The two are **not** the same — some terms parse but are
already normal forms.

**Parse and reduce** (locked by `testCase.ml`; exact normal forms in
[`examples/qam/README.md`](examples/qam/README.md)):

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

- **Bisimulation / observational equivalence** — not implemented (future work).
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

## Compiling QAM to OpenQASM (`compile.ml`)

`compile.ml` lowers a QAM configuration (`Ast.membrane list`) to a circuit,
following the paper's Figure 13 / Appendix E. It is **syntax-directed** — it
recurses on process/membrane structure and emits gates, independent of the
reduction relation.

| QAM action | Rule | Emitted |
|------------|------|---------|
| `nu c.` (left end of `c`) | C-CohereL | `h q[i]; cx q[i], q[j]` (Bell pair on the two parties' blanks) |
| `nu c.` (right end) | C-CohereR | *(passive; no gates)* |
| `c<-µ.` (µ a quantum resource) | Encode (Fig. 10 block) | `cx q[µ], q[i]; h q[µ]` — and remembers µ as `c`'s payload |
| `c<-µ.` (µ a classical residue) | Recover (Fig. 10 block) | `if(xb==1) x q[i]; if(zb==1) z q[i];` |
| `c<-µ.` (µ otherwise unknown) | classical input | declares `creg µ[1]` (input bit) + controlled X/Z — superdense's classical message |
| `c->x.` (decode) | Decode (Fig. 10 block) | measures the channel qubit and, if `c` carries a payload, the payload qubit; binds `x` to the (X bit, Z bit) residue |
| `c?y.` (receive) | C-Rev | synchronizer; on a quantum channel binds `y` to the channel, on a classical one to the sender's residue |
| `a!x.` (classical send) | Com | comment (ordering); records `x`'s residue on channel `a` |

**Layout (the paper's Σ, N = 1):** every resource molecule is one qubit;
membrane regions are consecutive. Blank `o` resources are claimed, in order, by
channel creation (the Cohere rule *requires* one blank per party — a missing
blank is a compile error), and named resources are addressable encode messages.
Each measurement gets its own 1-bit creg.

**Physical validation (`sim.ml`):** a minimal built-in statevector simulator for
the emitted QASM subset — measurements *branch* the run (a branch's squared norm
is its probability) and `if(creg==1)` gates apply per-branch. `dune test` checks
that Cohere really produces the Bell state (`test_bell_simulates`) and that the
compiled teleportation circuit reproduces the message state on Bob's qubit in
**every** measurement branch (`test_teleport_simulates`).

**Fidelity note (Fig. 13 vs Fig. 10):** encode/decode are lowered to the paper's
*concrete* teleportation circuit (Figure 10: CNOT then H on the **message**
qubit; measure both the channel qubit and the payload, giving separate X and Z
correction bits). Figure 13's C-EncodeQ/C-DecodeQ as literally written (H on the
*channel* qubit; measuring only the channel; a single classical bit) leaves the
message entangled with Bob's qubit and does not reproduce teleportation on a
statevector — the Fig. 10 lowering is the one that passes simulation.

**Known limits:** flat OpenQASM discards the QAM's locality/no-relocation
guarantee (a faithful, concurrent-IR target à la the paper's Concurrent SQIR is
future work); classical channels carry one residue and the sender's membrane
must precede the receiver's (send/wait are linearized, no scheduler); `N = 1`
qubit per message and one encode per channel; projective channels are resolved
syntactically (a receive-bound name stands for its channel), not semantically.

## Side demo: quantum chemistry / VQE (standalone)

Unrelated to the QAM calculus, the repo also carries a small self-contained
chemistry demo: `chemistry.ml` (tiny Hamiltonian DSL + toy single-parameter VQE
+ OpenQASM export) and `chem_main.ml`:

```bash
dune exec -- chem_main -- --benchmark
dune exec -- chem_main -- --dsl examples/chem/h2.qamchem
```

It shares no code with the QAM pipeline and is kept as a demonstration only.

## Repository layout

```
qam_ast/         canonical AST
grammar/         ocamllex + Menhir surface grammar
interpreter.ml   reductions, normalize, printer, REPL
checkEquivalence.ml  canonicalizer + structural equivalence
compile.ml       QAM -> OpenQASM 2.0 backend
sim.ml           statevector simulator (used by tests and `qam simulate`)
qam.ml           CLI entry point
examples/qam/    protocol examples + expected normal forms / circuits
archive/         quarantined semantics lab (REWRITE_NOPARSER) and legacy code;
                 excluded from the build
```

## Citing

If you use this software, please cite it (see [`CITATION.cff`](CITATION.cff))
along with the paper it implements:

> Liyi Li, Le Chang, Rance Cleaveland, Mingwei Zhu, Xiaodi Wu.
> *The Quantum Abstract Machine.* arXiv:2402.13469, 2024.

## License

[MIT](LICENSE) © Son Vu
