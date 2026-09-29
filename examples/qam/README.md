# QAM reduction examples

Each `.qam` file holds one membrane. The `.qam` surface syntax has no comment
form, so expected normal forms are documented here (and asserted in
`../../testCase.ml`). Normal forms are printed by `Interpreter.string_of_membrane`.

| File | Input | Rule exercised | Expected normal form |
|------|-------|----------------|----------------------|
| `encode.qam` | `{ a<-k., a }` | ENCODE | `{ 0, a & a }` |
| `cohere.qam` | `{ nu c., c!x. }` | COHERE | `{ 0, 0 }` |
| `decode.qam` | `{ c->x., c?y. }` | DECODE | `{ 0, 0 }` |

Notes:
- `0` is `NullProcess`; `a & a` is `MeetOperation (SimpleResource "a", SimpleResource "a")`,
  the resource ENCODE produces by meeting the target resource with the encode channel.
- All three normal forms parse back through the grammar (round-trip), which is
  why ENCODE's `&` output needed surface syntax.

## Protocol examples (Path B: compiled to OpenQASM, not reduced)

| File | Protocol (paper reference) | Compile with |
|------|---------------------------|--------------|
| `bitcommit.qam` | Bit-commitment (Example 1) | `qam compile examples/qam/bitcommit.qam` |
| `teleport.qam` | Quantum teleportation (Example 2) | `qam compile examples/qam/teleport.qam` |
| `superdense.qam` | Superdense coding (Example 21) | `qam compile examples/qam/superdense.qam` |

Expected circuits are locked by `test_compile_bit_commitment`,
`test_compile_teleportation`, and `test_compile_superdense` in
`../../testCase.ml`; physical correctness of teleportation is verified by the
built-in statevector simulator (`test_teleport_simulates`).
