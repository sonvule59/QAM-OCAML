# QAM protocol examples

Each `.qam` file holds one configuration (the surface syntax has no comment
form, so the protocols are documented here). Run them on the abstract machine
with `qam run FILE`, or compile with `qam compile FILE` (`--qasm3`,
`--concurrent`, `--width N`).

## Protocol examples (Path B: compiled to OpenQASM, not reduced)

| File | Protocol (paper reference) | Compile with |
|------|---------------------------|--------------|
| `bitcommit.qam` | Bit-commitment (Example 1) | `qam compile examples/qam/bitcommit.qam` |
| `teleport.qam` | Quantum teleportation (Example 2) | `qam compile examples/qam/teleport.qam` |
| `superdense.qam` | Superdense coding (Example 21) | `qam compile examples/qam/superdense.qam` |

| `qes.qam` | Quantum entanglement swap (Examples 9-10) | `qam run examples/qam/qes.qam` |

Expected circuits and machine terminals are locked in `../../testCase.ml`;
physical correctness of teleportation is verified by the built-in statevector
simulator, and the entanglement swap by `test_machine_entanglement_swap`
(terminal `{ c.o }, { c.o }` — Ann and Bob end up sharing channel c).
