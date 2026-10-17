#!/usr/bin/env python3
"""Run QAM-OCAML's compiled teleportation circuit on IBM Quantum hardware.

  qam compile examples/qam/teleport.qam > teleport.qasm

  python3 scripts/run_ibm.py teleport.qasm --fake
      Local dry-run on a noisy FAKE IBM device (no account needed): exercises
      the full hardware pipeline — device basis gates, coupling map, noise
      model. Implies --post-select (the fake device snapshot predates
      dynamic-circuit support).

  python3 scripts/run_ibm.py teleport.qasm [--backend NAME]
      Real hardware. Credentials: either save them once with
        python3 -c "from qiskit_ibm_runtime import QiskitRuntimeService; \\
          QiskitRuntimeService.save_account(channel='ibm_quantum_platform', token='YOUR_TOKEN')"
      or export QISKIT_IBM_TOKEN. Without --backend, picks the least busy
      operational device.

  --post-select
      Strip the classically-controlled corrections and post-select the
      m0=m1=0 branch instead — for backends without dynamic-circuit support.

The circuit teleports 0.6|0> + 0.8|1>: success is P(bob=1) ~= 0.64. A few
percent of deviation on hardware is the device's noise, not the compiler.
"""
import math
import os
import sys

from qiskit import QuantumCircuit, qasm2
from qiskit.circuit import ClassicalRegister
from qiskit.transpiler.preset_passmanagers import generate_preset_pass_manager

ALPHA, BETA = 0.6, 0.8
SHOTS = 4096


def build(path, post_select):
    core = qasm2.load(path)
    if post_select:
        stripped = QuantumCircuit(*core.qregs, *core.cregs)
        for inst in core.data:
            if inst.operation.name != "if_else":
                stripped.append(inst.operation, inst.qubits, inst.clbits)
        core = stripped
    prep = QuantumCircuit(*core.qregs, *core.cregs)
    prep.ry(2 * math.atan2(BETA, ALPHA), 0)  # message qubit
    full = prep.compose(core)
    out = ClassicalRegister(1, "out")
    full.add_register(out)
    full.measure(2, out[0])  # Bob's qubit
    return full


def get_backend(args):
    if "--fake" in args:
        from qiskit_ibm_runtime.fake_provider import FakeManilaV2

        return FakeManilaV2(), True
    from qiskit_ibm_runtime import QiskitRuntimeService

    token = os.environ.get("QISKIT_IBM_TOKEN")
    service = (
        QiskitRuntimeService(channel="ibm_quantum_platform", token=token)
        if token
        else QiskitRuntimeService()
    )
    if "--backend" in args:
        return service.backend(args[args.index("--backend") + 1]), False
    return service.least_busy(operational=True, simulator=False, min_num_qubits=3), False


def main():
    args = sys.argv[1:]
    if not args or args[0].startswith("-"):
        print(__doc__)
        sys.exit(2)
    path = args[0]
    fake = "--fake" in args
    post_select = "--post-select" in args or fake
    if fake and "--post-select" not in args:
        print("note    : --fake implies --post-select (device snapshot lacks dynamic circuits)")
    circ = build(path, post_select)
    backend, is_fake = get_backend(args)
    print(f"backend : {backend.name}")
    try:
        tqc = generate_preset_pass_manager(backend=backend, optimization_level=1).run(circ)
    except Exception as e:
        if "if_else" in str(e):
            print("this backend lacks dynamic-circuit support; re-run with --post-select")
            sys.exit(1)
        raise
    from qiskit_ibm_runtime import SamplerV2

    job = SamplerV2(mode=backend).run([tqc], shots=SHOTS)
    if not is_fake:
        print(f"job id  : {job.job_id()}")
    data = job.result()[0].data
    m0 = data.m0.get_bitstrings()
    m1 = data.m1.get_bitstrings()
    out = data.out.get_bitstrings()
    shots = len(out)
    if post_select:
        kept = [o for a, b, o in zip(m0, m1, out) if a == "0" and b == "0"]
        print(f"post-selected m0=m1=0: {len(kept)}/{shots} shots")
        p1 = kept.count("1") / max(len(kept), 1)
    else:
        p1 = out.count("1") / shots
        for a in "01":
            for b in "01":
                sel = [o for x, y, o in zip(m0, m1, out) if x == a and y == b]
                if sel:
                    print(
                        f"  branch m0={a},m1={b}: n={len(sel):5d}  P(bob=1)={sel.count('1') / len(sel):.4f}"
                    )
    print(f"P(bob=1) = {p1:.4f}   (ideal {BETA ** 2:.4f})")
    delta = abs(p1 - BETA**2)
    verdict = (
        "exact (noiseless)"
        if delta < 0.02
        else "teleported, within hardware noise"
        if delta < 0.12
        else "DEGRADED — check backend calibration / qubit selection"
    )
    print(f"verdict : {verdict} (delta = {delta:.4f})")
    sys.exit(0 if delta < 0.12 else 1)


if __name__ == "__main__":
    main()
