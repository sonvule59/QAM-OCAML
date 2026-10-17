# Changelog

## [0.3.0] — 2026-10-10

First tagged release: a complete, tested implementation of Li, Chang,
Cleaveland, Zhu and Wu, *The Quantum Abstract Machine* (arXiv:2402.13469).

### Language & front-end
- Menhir surface grammar: membranes, molecules, sequenced action prefixes,
  choice, replication, airlocks, meet `&`, null process `0`, named
  `let ... ;` definitions; positioned parse errors.
- Static checker (`typecheck.ml`): channel-kind inference (Ω),
  well-formedness (two parties per channel, blank accounting), static
  no-cloning (linear resources, decode-once, one quantum payload per channel).

### Semantics
- The paper's machine as a configuration-level LTS (`semantics.ml`):
  Cohere, Encode+QLocal, Decode+AirEQ (projective channels, residues,
  substitution), CLocal, Com, CL/CR, MT/NT, Split/Decohere (flagged),
  the meet algebra ⊙ (Def. 5) with key-lock recovery.
- Executes the paper's protocols to their exact terminals: teleportation,
  bit-commitment, superdense coding, quantum entanglement swap.
- Equivalence = trace refinement (Defs. 14–15); exact for replication-free
  configurations, depth-bounded otherwise.
- Extended QAM (Sec. 4, `network.ml`): located membranes, θ success-rate
  graphs, channel intentions, QPass/QCast routing predicates, likeliness
  trace refinement (Defs. 16–19).

### Compiler
- Circuit IR (`ir.ml`) with per-membrane programs and first-class send/wait;
  fixpoint classical-dataflow linking; blocking scheduler with deadlock
  detection; gate-cancellation and dead-qubit passes; configurable message
  width (the paper's bandwidth N).
- Three backends: flat OpenQASM 2.0, OpenQASM 3.0 (bits, if-statements,
  input parameters), and the concurrent target (b) — the paper's Concurrent
  SQIR shape, executable on the built-in concurrent runtime.

### Verification
- Built-in branching statevector simulator (`sim.ml`); teleportation verified
  physically on both flat and concurrent runtimes, in either membrane order.
- Seeded property suite (`propTest.ml`): totality, dynamic no-cloning
  invariants, trace laws, and Theorem-28-style machine/circuit agreement on
  randomized teleportation variants; mutation-tested.
- External validation on Qiskit Aer (`scripts/validate_qiskit.py`), including
  transpilation to IBM's `{rz,sx,x,cx}` hardware basis, and an IBM Quantum
  hardware runner (`scripts/run_ibm.py`) with a no-account noisy-fake dry run.

### Documented deviations from the paper
- Encode/decode lower to the concrete Figure 10 teleportation circuit;
  Figure 13's C-EncodeQ/C-DecodeQ as literally written does not reproduce
  teleportation on a statevector.
- Def. 5 leaves the projective label's fate implicit and the worked examples
  differ: quantum payloads shed the label (Ex. 8), classical payloads keep it
  (Ex. 21); both behaviors implemented as the examples dictate.
