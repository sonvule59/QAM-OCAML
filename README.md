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
qam check FILE.qam      typecheck (channel kinds, well-formedness, no-cloning)
qam run FILE.qam [N]    execute the abstract machine (LTS) to its terminals
qam compile FILE.qam    typecheck, then compile to OpenQASM 2.0
    --qasm3                 ... to OpenQASM 3.0 (bits, if-statements, input parameters)
    --concurrent            ... to the concurrent circuit, target (b)
qam simulate FILE.qam   compile, then run on the built-in statevector simulator
    --concurrent            ... on the concurrent runtime (per-process, blocking send/wait)
qam repl                interactive reduction / equivalence REPL (legacy fragment)
```

(From a clone, prefix with `dune exec ./qam.exe --`; or `./run.sh <args>`.)

## The pipeline

| Stage | Where |
|-------|-------|
| Lexer / Menhir grammar → `Ast.membrane list` (definitions elaborated away) | `grammar/`, `qam_ast/ast.ml` |
| Static checker: channel-kind inference (Ω), well-formedness, static no-cloning | `typecheck.ml` |
| **The abstract machine**: configuration-level LTS with the paper's rules, meet algebra ⊙, traces + trace refinement | `semantics.ml` |
| Legacy single-membrane reducer + term printer (kept for the REPL and canon-equivalence fragment) | `interpreter.ml` |
| **Circuit IR**: per-membrane instruction lists with first-class send/wait; scheduling, deadlock detection, peephole + dead-qubit passes; three backends (QASM 2, QASM 3, concurrent target (b)) | `ir.ml` |
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

**Named definitions** let protocols read like the paper's examples:

```text
let Alice = nu c.c<-d.c->x.a!x. ;
let Bob = nu c.c?u.a?z.u<-z. ;
{ Alice, d, o }, { Bob, o }
```

Definitions are substituted away right after parsing. A bare name in molecule
position parses as a resource and is promoted to the defined process when a
matching `let` exists; an unresolved reference is a type error.

## Static checking (`typecheck.ml`)

`qam check` (also run automatically by `compile`/`simulate`) rejects ill-formed
protocols before codegen, with positioned errors:

- **Channel kinds (the paper's Ω):** ν-created / encoded / decoded channels are
  quantum; `!`-sent channels are classical; receive is polymorphic. Conflicts
  (e.g. `nu c.` then `c!x.`) are errors.
- **Well-formedness (paper Def. 23):** every quantum channel has exactly two
  creating membranes, and each creating membrane holds a blank `o` for the
  Cohere rule to consume.
- **Static no-cloning (paper Thm. 3):** a named quantum resource is consumed by
  at most one encode; a channel party is used as a quantum message at most
  once; a channel is decoded at most once; a decode's classical residue cannot
  be used as a channel.

```text
$ qam check bad.qam
Type error: line 1, column 13: no-cloning violation: quantum resource d in membrane 0 is consumed 2 times
```

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

## The abstract machine (`semantics.ml`)

`semantics.ml` implements the paper's reduction semantics as a
configuration-level **labeled transition system**:

```
step : membrane list -> (label * membrane list) list
```

Every successor of every redex is returned — Choice explores **both** branches
(rules CL *and* CR), so the legacy commit-left limitation does not apply here.
Implemented rules: **Cohere** (inter-membrane, consumes a blank `◦` per party,
produces `c.◦`, label `c`), **Encode** with **QLocal** fused, **Decode** fused
with **AirEQ** (pools the two parties' content, binds the classical residue
`^c.mu` on the decoder side and the projective channel `^c` plus quantum
residue `^c.~mu` on the receiver side), **CLocal**, **Com** (label `a.i`),
**MT/NT** (replication, ν-binders freshened), **ID1/ID2** as tidying, and —
behind flags, off by default because they explode the state space — **Split**
and **Decohere**. Projective channels are ordinary names prefixed `^`; `~m`
prints the residue `†m`.

The **meet algebra ⊙ (Def. 5)** is implemented as directed rewrites, including
the key-lock recovery `^c.q ⊙ ^c.~q ≡ q`. Fidelity note: Def. 5 leaves the fate
of the projective label implicit, and the paper's worked examples differ — a
*quantum* payload sheds the label (Example 8: teleportation recovers `d.e`
plain), while a *classical* payload keeps it so CLocal can deliver it
(Example 21: `^c.(^c.i ⊙ ~i) ≡ ^c.i`). We implement exactly that distinction.

**The machine executes the paper's protocols** (`qam run`):

```text
$ qam run examples/qam/teleport.qam
initial : { nu c.c<-d.c->x.a!x., d, o }, { nu c.c?u.a?z.u<-z., o }
terminal: { d.o }
  trace : [c; ^c.d.o; a.^c.d.o]
```

Bob ends holding the message `d` — teleportation, executed abstractly, with the
paper's label trace (Cohere; Decode; Com). Superdense coding terminates to the
empty configuration ({|0|},{|0|}, Example 21) and bit-commitment leaves Bob the
quantum residue `^c.~o` (Example 1). Locked by `test_machine_*`.

**Traces and refinement (Defs. 14–15):** `traces ~depth` computes the
prefix-closed set of visible-label traces over at most `depth` steps, and
`trace_refines ~depth c1 c2` checks `T(c1) ⊆ T(c2)` at equal depth. For
**replication-free** configurations the LTS is finite (every rule strictly
consumes), so `traces_exact` / `trace_refines_exact` compute the *unbounded*
sets — refinement is exact there. With `repl` the LTS is infinite-state and
only the bounded approximation is computable (exact inclusion is undecidable
in general); `traces_exact` rejects such configurations rather than pretend.
The legacy normalize-then-canonicalize equivalence (above) remains for the
older fragment; trace refinement is the machine-level notion.

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

**Compiler architecture (Phase 3):** compilation is now a real pipeline —
lower → IR → passes → emit. Lowering produces one instruction list **per
membrane** (`ir.ml`), with classical `send`/`wait` as first-class instructions;
classical dataflow is linked by **fixpoint rounds**, so membranes may appear in
any order (a receiver listed before its sender compiles — and still passes the
teleportation simulation, `test_simulate_receiver_first`). The flat-QASM
backend then runs passes:

- **scheduling** — list-schedules the per-membrane programs, blocking each
  `wait` until its channel's `send` has been emitted; an unservable wait is a
  reported **classical synchronization deadlock** (`test_compile_deadlock_detected`);
- **gate cancellation** — adjacent self-inverse pairs (H H, X X, Z Z, CX CX)
  cancel to a fixpoint;
- **dead-qubit elimination** — unused resources no longer waste register slots.

**Backends (Phase 4):** three emitters share the IR.

| Target | Command | What it is |
|--------|---------|------------|
| OpenQASM 2.0 (default) | `qam compile f.qam` | flat circuit, runs on any QASM simulator/hardware stack |
| OpenQASM 3.0 | `qam compile f.qam --qasm3` | real classical types: `bit m0; m0 = measure q[1]; if (m0 == 1) { x q[2]; }`, and superdense's classical message becomes a genuine `input bit i;` runtime parameter |
| **Concurrent, target (b)** | `qam compile f.qam --concurrent` | the paper's Concurrent SQIR shape: one process per membrane, pinned to its Σ region (`process 0 owns q[0,1] { ... }`), shared quantum state, explicit `send`/`wait` — **no scheduling pass**, locality preserved |

The concurrent target is **executable**: `qam simulate f.qam --concurrent` runs
`Sim.run_ir`, a runtime scheduler that interleaves the per-membrane processes
against the shared branching statevector, blocking each `wait` until its
`send` has executed (a circular wait raises a deadlock failure). Teleportation
passes the same physics checks on the concurrent runtime as on the flat
backend — in either membrane order (`test_concurrent_sim_*`).

**Locality check:** `Ir.cross_region` lists every instruction that touches
another membrane's region. For teleportation it reports exactly one — Cohere's
collaborative Bell-pair `cx` — matching the paper's reading that channel
creation is the only joint operation (`test_concurrent_locality`).

**Known limits:** classical channels carry one residue; `N = 1` qubit per
message and one encode per channel; projective channels are resolved
syntactically (a receive-bound name stands for its channel), not semantically;
codegen commits Choice to the left branch (the abstract machine explores
both); the concurrent runtime executes one valid interleaving (protocol
synchronization makes the result interleaving-independent for the supported
fragment).

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
