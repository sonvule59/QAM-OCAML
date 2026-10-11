#!/usr/bin/env python3
"""Validate QAM-OCAML's emitted OpenQASM 2.0 on Qiskit Aer.

The qam compiler emits the *protocol core* (starting from |0...0>, no final
measurement of the receiver). This harness wraps it the way a real user would:
prepare an input state on the message qubit, append a measurement of the
receiver's qubit, run on Aer (also transpiled to IBM's {rz,sx,x,cx} hardware
basis), and check the output statistics.

Usage:
  qam compile examples/qam/teleport.qam  > teleport.qasm
  qam compile examples/qam/bitcommit.qam > bitcommit.qasm
  python3 scripts/validate_qiskit.py teleport.qasm bitcommit.qasm
"""
import math
import sys

from qiskit import QuantumCircuit, qasm2, transpile
from qiskit.circuit import ClassicalRegister
from qiskit_aer import AerSimulator

SHOTS = 20000
ALPHA, BETA = 0.6, 0.8  # message state 0.6|0> + 0.8|1>


def run(circ, basis=None):
    sim = AerSimulator()
    tqc = transpile(circ, sim, basis_gates=basis, optimization_level=1)
    return sim.run(tqc, shots=SHOTS).result().get_counts()


def check(name, ok, detail):
    print(f"  {'PASS' if ok else 'FAIL'}  {name}: {detail}")
    return ok


def validate_teleport(path):
    print(f"== teleportation: {path} ==")
    core = qasm2.load(path)
    prep = QuantumCircuit(*core.qregs, *core.cregs)
    theta = 2 * math.atan2(BETA, ALPHA)
    prep.ry(theta, 0)  # message qubit (measured into m1 by the decode)
    full = prep.compose(core)
    out = ClassicalRegister(1, "out")
    full.add_register(out)
    full.measure(2, out[0])  # Bob's qubit
    ok = True
    for label, basis in [("aer", None), ("ibm basis {rz,sx,x,cx}", ["rz", "sx", "x", "cx"])]:
        counts = run(full, basis)
        # creg key order is reverse of addition: "out m1 m0"
        total_1 = sum(v for k, v in counts.items() if k.split()[0] == "1")
        p1 = total_1 / SHOTS
        ok &= check(f"P(bob=1) = |beta|^2 [{label}]", abs(p1 - BETA**2) < 0.02, f"{p1:.4f} vs {BETA**2:.4f}")
        # every (m0,m1) branch ~uniform 1/4, and bob correct in each branch
        for m1 in "01":
            for m0 in "01":
                br = {k: v for k, v in counts.items() if k.split()[1] == m1 and k.split()[2] == m0}
                n = sum(br.values())
                ok &= check(f"branch m0={m0},m1={m1} prob [{label}]", abs(n / SHOTS - 0.25) < 0.02, f"{n / SHOTS:.4f}")
                b1 = sum(v for k, v in br.items() if k.split()[0] == "1") / max(n, 1)
                ok &= check(f"P(bob=1 | m0={m0},m1={m1}) [{label}]", abs(b1 - BETA**2) < 0.03, f"{b1:.4f}")
    return ok


def validate_bitcommit(path):
    print(f"== bit-commitment: {path} ==")
    core = qasm2.load(path)
    full = QuantumCircuit(*core.qregs, *core.cregs).compose(core)
    out = ClassicalRegister(1, "out")
    full.add_register(out)
    full.measure(1, out[0])  # Bob's half of the Bell pair
    counts = run(full)
    agree = sum(v for k, v in counts.items() if k.split()[0] == k.split()[1])
    p0 = sum(v for k, v in counts.items() if k.split()[1] == "0") / SHOTS
    ok = check("Bob's bit == Alice's residue (Bell correlation)", agree == SHOTS, f"{agree}/{SHOTS}")
    ok &= check("P(m0=0) uniform", abs(p0 - 0.5) < 0.02, f"{p0:.4f}")
    return ok


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(__doc__)
        sys.exit(2)
    ok = validate_teleport(sys.argv[1])
    ok &= validate_bitcommit(sys.argv[2])
    print("ALL PASS" if ok else "FAILURES")
    sys.exit(0 if ok else 1)
