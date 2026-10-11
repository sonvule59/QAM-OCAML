open OUnit2
open Ast
open CheckEquivalence
open Interpreter
open Chemistry

let test_check_resource_equivalence _ =
  let r1 = SimpleResource "a" in
  let r2 = SimpleResource "a" in
  let r3 = SimpleResource "b" in
  assert_bool "same named resource" (check_resource_equivalence r1 r2);
  assert_bool "different named resource" (not (check_resource_equivalence r1 r3))

let test_check_process_equivalence _ =
  let p1 = ActionProcess (mk_action (Send { chan = "a"; arg = Named "x" }), NullProcess) in
  let p2 = ActionProcess (mk_action (Send { chan = "a"; arg = Named "x" }), NullProcess) in
  let p3 = ActionProcess (mk_action (Receive { chan = "a"; arg = "x" }), NullProcess) in
  assert_bool "same process" (check_process_equivalence p1 p2);
  assert_bool "different process action" (not (check_process_equivalence p1 p3))

let test_check_molecule_equivalence _ =
  let m1 = ProcessMolecule (ActionProcess (mk_action (Encode { chan = "a"; msg = Named "k" }), NullProcess)) in
  let m2 = ProcessMolecule (ActionProcess (mk_action (Encode { chan = "a"; msg = Named "k" }), NullProcess)) in
  let m3 = ResourceMolecule (SimpleResource "a") in
  assert_bool "same molecule" (check_molecule_equivalence m1 m2);
  assert_bool "different molecule kinds" (not (check_molecule_equivalence m1 m3))

let test_check_membrane_equivalence _ =
  let left =
    MoleculeMembrane
      [
        ProcessMolecule (ActionProcess (mk_action (Send { chan = "a"; arg = Named "x" }), NullProcess));
        ResourceMolecule (SimpleResource "a");
      ]
  in
  let same =
    MoleculeMembrane
      [
        ProcessMolecule (ActionProcess (mk_action (Send { chan = "a"; arg = Named "x" }), NullProcess));
        ResourceMolecule (SimpleResource "a");
      ]
  in
  let different =
    MoleculeMembrane
      [
        ProcessMolecule (ActionProcess (mk_action (Receive { chan = "a"; arg = "x" }), NullProcess));
        ResourceMolecule (SimpleResource "a");
      ]
  in
  assert_bool "same membrane structure" (check_membrane_equivalence left same);
  assert_bool
    "different membrane structure"
    (not (check_membrane_equivalence left different))

(** Parser regression: lexer + menhir grammar *)
let test_parser_choice_and_repl _ =
  let ok s =
    match parse_membranes_from_string s with
    | Ok _ -> ()
    | Error _ ->
        assert_failure ("parse regression failed for sample: " ^ s)
  in
  ok "{ a!bc. + d?ea.}";
  ok "{ repl a!bq.}"

let read_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* ---- Path B / target (a): QAM -> flat OpenQASM (Compile.compile_to_qasm) ---- *)
let contains_sub s sub =
  let sl = String.length s and bl = String.length sub in
  let rec go i =
    if i + bl > sl then false
    else if String.sub s i bl = sub then true
    else go (i + 1)
  in
  go 0

let qasm_of_file path =
  match parse_membranes_from_string (read_file path) with
  | Ok cfg -> Compile.compile_to_qasm cfg
  | Error _ -> assert_failure ("parse failed for " ^ path)

(* Cohere: two membranes creating channel c (each holding a blank `o`, as the
   paper's Cohere rule requires) compile to a Bell pair (C-CohereL/C-CohereR). *)
let test_compile_bell_pair _ =
  match parse_membranes_from_string "{ nu c., o }, { nu c., o }" with
  | Ok cfg ->
      let q = Compile.compile_to_qasm cfg in
      assert_bool "qreg q[2]" (contains_sub q "qreg q[2];");
      assert_bool "H on q[0]" (contains_sub q "h q[0];");
      assert_bool "CX q[0],q[1]" (contains_sub q "cx q[0], q[1];")
  | Error _ -> assert_failure "parse failed"

(* The paper's Cohere rule requires a blank in each party; without one the
   compiler refuses to fabricate a qubit. *)
let test_compile_missing_blank _ =
  match parse_membranes_from_string "{ nu c. }, { nu c. }" with
  | Ok cfg ->
      let q = Compile.compile_to_qasm cfg in
      assert_bool "reports missing blank" (contains_sub q "// ERROR: channel c needs a blank")
  | Error _ -> assert_failure "parse failed"

(* Bit-commitment (paper Example 1), now written in surface syntax thanks to
   action-prefix sequencing: Alice = nu c.c->x. ; Bob = nu c.c?y. *)
let test_compile_bit_commitment _ =
  let q = qasm_of_file "examples/qam/bitcommit.qam" in
  assert_bool "OPENQASM header" (String.starts_with ~prefix:"OPENQASM 2.0;" q);
  assert_bool "Bell H" (contains_sub q "h q[0];");
  assert_bool "Bell CX" (contains_sub q "cx q[0], q[1];");
  assert_bool "Alice decode measures q[0]" (contains_sub q "measure q[0] -> m0[0];")

(* Quantum teleportation (paper Example 2). Layout matches the paper's
   Appendix E: message q[0], Alice's Bell half q[1], Bob's q[2]. Encode/decode
   are lowered to the paper's concrete Figure 10 circuit. *)
let test_compile_teleportation _ =
  let q = qasm_of_file "examples/qam/teleport.qam" in
  assert_bool "3 qubits" (contains_sub q "qreg q[3];");
  assert_bool "Bell pair q[1],q[2]" (contains_sub q "cx q[1], q[2];");
  assert_bool "encode CNOT msg->channel" (contains_sub q "cx q[0], q[1];");
  assert_bool "encode H on message" (contains_sub q "h q[0];");
  assert_bool "decode measures channel" (contains_sub q "measure q[1] -> m0[0];");
  assert_bool "decode measures payload" (contains_sub q "measure q[0] -> m1[0];");
  assert_bool "X correction on Bob" (contains_sub q "if(m0==1) x q[2];");
  assert_bool "Z correction on Bob" (contains_sub q "if(m1==1) z q[2];")

(* Superdense coding (paper Example 21): the classical message becomes a 1-bit
   input creg; Bob recovers via classically-controlled corrections. *)
let test_compile_superdense _ =
  let q = qasm_of_file "examples/qam/superdense.qam" in
  assert_bool "classical input creg" (contains_sub q "creg i[1];");
  assert_bool "Bell pair" (contains_sub q "cx q[0], q[1];");
  assert_bool "input-controlled X" (contains_sub q "if(i==1) x q[0];");
  assert_bool "input-controlled Z" (contains_sub q "if(i==1) z q[0];");
  assert_bool "decode" (contains_sub q "measure q[0] -> m0[0];");
  assert_bool "Bob X correction" (contains_sub q "if(m0==1) x q[1];")

(* ---- Physical validation: built-in statevector simulation (sim.ml) ---- *)

(* Cohere alone must produce the Bell state (|00> + |11>)/sqrt2. *)
let test_bell_simulates _ =
  match parse_membranes_from_string "{ nu c., o }, { nu c., o }" with
  | Ok cfg -> (
      match Sim.run (Compile.compile_to_qasm cfg) with
      | [ br ] ->
          let s = 1.0 /. sqrt 2.0 in
          let close c v =
            Float.abs (c.Complex.re -. v) < 1e-9 && Float.abs c.Complex.im < 1e-9
          in
          assert_bool "amp |00>" (close br.Sim.amp.(0) s);
          assert_bool "amp |11>" (close br.Sim.amp.(3) s);
          assert_bool "amp |01>,|10> zero"
            (Complex.norm br.Sim.amp.(1) < 1e-9 && Complex.norm br.Sim.amp.(2) < 1e-9)
      | _ -> assert_failure "expected exactly one branch (no measurement)")
  | Error _ -> assert_failure "parse failed"

(* The compiled teleportation circuit must reproduce the message state
   alpha|0> + beta|1> on Bob's qubit in EVERY measurement branch, each branch
   with probability 1/4. This is the end-to-end physical correctness check. *)
let test_teleport_simulates _ =
  let qasm = qasm_of_file "examples/qam/teleport.qam" in
  let alpha = 0.6 and beta = 0.8 in
  let branches =
    Sim.run
      ~init:(0, ({ Complex.re = alpha; im = 0.0 }, { Complex.re = beta; im = 0.0 }))
      qasm
  in
  assert_equal ~printer:string_of_int 4 (List.length branches);
  List.iter
    (fun br ->
      (* m1 holds q0's bit, m0 holds q1's bit; Bob is q2 (bit value 4) *)
      let bit r = match List.assoc_opt r br.Sim.cregs with Some v -> v | None -> 0 in
      let base = bit "m1" + (2 * bit "m0") in
      let a0 = br.Sim.amp.(base) and a1 = br.Sim.amp.(base + 4) in
      let p = Complex.norm2 a0 +. Complex.norm2 a1 in
      assert_bool "branch probability 1/4" (Float.abs (p -. 0.25) < 1e-9);
      let cross =
        Complex.sub
          (Complex.mul a0 { Complex.re = beta; im = 0.0 })
          (Complex.mul a1 { Complex.re = alpha; im = 0.0 })
      in
      assert_bool "Bob's qubit carries the message state" (Complex.norm cross < 1e-9))
    branches

(* Action-prefix sequencing regression (grammar). *)
let test_parser_sequencing _ =
  match parse_membranes_from_string "{ nu c.c<-d.c->x.a!x., d, o }" with
  | Ok [ m ] ->
      assert_equal ~printer:(fun s -> s) "{ nu c.c<-d.c->x.a!x., d, o }"
        (string_of_membrane m)
  | _ -> assert_failure "sequenced prefixes failed to parse"

(* ---- Phase 1: static checker (typecheck.ml) ---- *)
let check_str s =
  match parse_membranes_from_string s with
  | Ok m -> Typecheck.check m
  | Error _ -> assert_failure ("parse failed: " ^ s)

let assert_checks s =
  match check_str s with
  | Ok _ -> ()
  | Error (e :: _) ->
      assert_failure ("expected well-typed: " ^ s ^ " — " ^ Typecheck.string_of_error e)
  | Error [] -> assert_failure "empty error list"

let assert_rejects ~containing s =
  match check_str s with
  | Ok _ -> assert_failure ("expected a type error for: " ^ s)
  | Error es ->
      assert_bool
        ("error should mention '" ^ containing ^ "'")
        (List.exists
           (fun e -> contains_sub (Typecheck.string_of_error e) containing)
           es)

let test_typecheck_protocols _ =
  List.iter
    (fun f ->
      match parse_membranes_from_string (read_file f) with
      | Ok m -> (
          match Typecheck.check m with
          | Ok _ -> ()
          | Error (e :: _) ->
              assert_failure (f ^ ": " ^ Typecheck.string_of_error e)
          | Error [] -> assert_failure "empty error list")
      | Error _ -> assert_failure ("parse failed: " ^ f))
    [ "examples/qam/bitcommit.qam"; "examples/qam/teleport.qam";
      "examples/qam/superdense.qam" ]

(* Omega inference: teleport's c is quantum, a is classical *)
let test_typecheck_omega _ =
  match parse_membranes_from_string (read_file "examples/qam/teleport.qam") with
  | Ok m -> (
      match Typecheck.check m with
      | Ok kinds ->
          assert_equal (Some Typecheck.KQuantum) (Hashtbl.find_opt kinds "c");
          assert_equal (Some Typecheck.KClassical) (Hashtbl.find_opt kinds "a")
      | Error _ -> assert_failure "teleport should typecheck")
  | Error _ -> assert_failure "parse failed"

let test_typecheck_no_cloning _ =
  assert_rejects ~containing:"no-cloning"
    "{ nu c.c<-d.c<-d., d, o }, { nu c.c?y., o }"

let test_typecheck_channel_arity _ =
  assert_rejects ~containing:"only one membrane" "{ nu c., o }";
  assert_rejects ~containing:"exactly two parties"
    "{ nu c., o }, { nu c., o }, { nu c., o }"

let test_typecheck_kind_conflict _ =
  assert_rejects ~containing:"used as classical" "{ nu c.c!x., o }, { nu c., o }"

let test_typecheck_missing_blank _ =
  assert_rejects ~containing:"blank 'o'" "{ nu c. }, { nu c., o }"

let test_typecheck_double_decode _ =
  assert_rejects ~containing:"decoded 2 times"
    "{ nu c.c->x.c->y., o }, { nu c.c?u., o }"

let test_typecheck_residue_as_channel _ =
  assert_rejects ~containing:"classical residue"
    "{ nu c.c->x.x<-d., d, o }, { nu c.c?u., o }"

(* the checker accepts the well-typed variant of the rejected shapes *)
let test_typecheck_accepts_valid _ =
  assert_checks "{ nu c.c<-d.c->x., d, o }, { nu c.c?y., o }"

(* one quantum payload per channel: "a quantum channel can only be used to
   teleport one message" (paper, Sec. 2.2) *)
let test_typecheck_one_payload _ =
  assert_rejects ~containing:"quantum payloads"
    "{ nu c.c<-d.c<-e.c->x., d, e, o }, { nu c.c?y., o }"

(* N = 2 qubits per message (the paper's bandwidth N, Appendix E): layout
   widens, gates loop per message qubit, residues carry two bits each. *)
let test_width2_teleport _ =
  let q =
    match parse_membranes_from_string (read_file "examples/qam/teleport.qam") with
    | Ok cfg -> Compile.compile_to_qasm ~width:2 cfg
    | Error _ -> assert_failure "parse failed"
  in
  assert_bool "6 qubits" (contains_sub q "qreg q[6];");
  assert_bool "second Bell pair" (contains_sub q "cx q[3], q[5];");
  assert_bool "second encode CNOT" (contains_sub q "cx q[1], q[3];");
  assert_bool "4 measurements" (contains_sub q "measure q[1] -> m3[0];");
  assert_bool "second X correction" (contains_sub q "if(m1==1) x q[5];");
  assert_bool "second Z correction" (contains_sub q "if(m3==1) z q[5];");
  (* physics: init message qubit 0 to 0.6|0>+0.8|1>; Bob's q[4] must carry it
     in every one of the 16 branches *)
  let alpha = 0.6 and beta = 0.8 in
  let branches =
    Sim.run ~init:(0, ({ Complex.re = alpha; im = 0.0 }, { Complex.re = beta; im = 0.0 })) q
  in
  assert_equal ~printer:string_of_int 16 (List.length branches);
  List.iter
    (fun br ->
      let bit r = match List.assoc_opt r br.Sim.cregs with Some v -> v | None -> 0 in
      let base =
        List.fold_left
          (fun acc (r, qb) -> if bit r = 1 then acc lor (1 lsl qb) else acc)
          0
          [ ("m0", 2); ("m1", 3); ("m2", 0); ("m3", 1) ]
      in
      let a0 = br.Sim.amp.(base) and a1 = br.Sim.amp.(base lor 16) in
      let p = Complex.norm2 a0 +. Complex.norm2 a1 in
      assert_bool "branch probability 1/16" (Float.abs (p -. (1.0 /. 16.0)) < 1e-9);
      let cross =
        Complex.sub
          (Complex.mul a0 { Complex.re = beta; im = 0.0 })
          (Complex.mul a1 { Complex.re = alpha; im = 0.0 })
      in
      assert_bool "Bob's first qubit carries the message" (Complex.norm cross < 1e-9))
    branches

(* ---- Phase 1 Stage 3: named process definitions -------------------------- *)

(* `let`-definitions elaborate away and compile to exactly the same circuit *)
let test_definitions_compile_equal _ =
  assert_equal ~printer:(fun s -> s)
    (qasm_of_file "examples/qam/teleport.qam")
    (qasm_of_file "examples/qam/teleport_defs.qam")

(* a reference in continuation position (`nu c.A`) parses and elaborates *)
let test_definitions_in_continuation _ =
  let src = "let A = c->x. ;\n{ nu c.A, o }, { nu c.c?y., o }" in
  match parse_membranes_from_string src with
  | Ok [ m; _ ] ->
      assert_equal ~printer:(fun s -> s) "{ nu c.c->x., o }" (string_of_membrane m)
  | _ -> assert_failure "definition in continuation failed to parse/elaborate"

(* an unresolved reference is a type error, not a silent no-op *)
let test_definitions_undefined_ref _ =
  assert_rejects ~containing:"undefined process reference"
    "let A = nu c.B ;\n{ A, o }, { nu c., o }"

(* ---- Phase 2: the abstract machine (semantics.ml) ------------------------ *)

let machine_terminals path depth =
  match parse_membranes_from_string (read_file path) with
  | Ok cfg ->
      List.map
        (fun (t, tr) -> (Semantics.string_of_config t, tr))
        (Semantics.reachable_terminals ~depth cfg)
  | Error _ -> assert_failure ("parse failed: " ^ path)

(* Teleportation executes in the machine itself (paper Examples 2/4/6/8):
   unique terminal — Bob holds the message d — with the paper's label trace
   Cohere(c); Decode(^c.d.o); Com(a.^c.d.o). *)
let test_machine_teleportation _ =
  match machine_terminals "examples/qam/teleport.qam" 14 with
  | [ (t, tr) ] ->
      assert_equal ~printer:(fun s -> s) "{ d.o }" t;
      assert_equal ~printer:(String.concat ";")
        [ "c"; "^c.d.o"; "a.^c.d.o" ] tr
  | ts -> assert_failure (Printf.sprintf "expected 1 terminal, got %d" (List.length ts))

(* Superdense coding terminates fully (paper Example 21): {|0|},{|0|}. *)
let test_machine_superdense _ =
  match machine_terminals "examples/qam/superdense.qam" 16 with
  | [ (t, tr) ] ->
      assert_equal ~printer:(fun s -> s) "(empty)" t;
      assert_equal ~printer:(String.concat ";") [ "c"; "^c.i"; "a.^c.i" ] tr
  | ts -> assert_failure (Printf.sprintf "expected 1 terminal, got %d" (List.length ts))

(* Bit-commitment (paper Example 1): Bob keeps the quantum residue ^c.~o. *)
let test_machine_bitcommit _ =
  match machine_terminals "examples/qam/bitcommit.qam" 10 with
  | [ (t, tr) ] ->
      assert_equal ~printer:(fun s -> s) "{ ^c.~o }" t;
      assert_equal ~printer:(String.concat ";") [ "c"; "^c.o" ] tr
  | ts -> assert_failure (Printf.sprintf "expected 1 terminal, got %d" (List.length ts))

(* CL and CR: the machine explores BOTH branches of a choice (unlike the
   legacy commit-left reducer). *)
let test_machine_choice_both_branches _ =
  match parse_membranes_from_string "{ a!x. + b!y. }, { a?u. }, { b?v. }" with
  | Ok cfg ->
      assert_equal ~printer:(String.concat " | ")
        [ ""; "a.x"; "b.y" ]
        (Semantics.traces ~depth:4 cfg)
  | Error _ -> assert_failure "parse failed"

(* Bounded trace refinement (paper Defs. 14-15). *)
let test_machine_trace_refinement _ =
  let parse s =
    match parse_membranes_from_string s with
    | Ok c -> c
    | Error _ -> assert_failure ("parse failed: " ^ s)
  in
  let choice = parse "{ a!x. + b!y. }, { a?u. }, { b?v. }" in
  let small = parse "{ a!x. }, { a?u. }" in
  assert_bool "deterministic refines nondeterministic"
    (Semantics.trace_refines ~depth:4 small choice);
  assert_bool "nondeterministic does not refine deterministic"
    (not (Semantics.trace_refines ~depth:4 choice small));
  assert_bool "refinement is reflexive" (Semantics.trace_refines ~depth:6 choice choice)

(* The meet algebra ⊙ (paper Def. 5). *)
let test_machine_meet_algebra _ =
  let d = Channeled ("d", Blank) in
  (* identity: ◦ ⊙ q ≡ q *)
  assert_equal (Named "q") (Semantics.meet_normalize (Meet (Blank, Named "q")));
  (* quantum key-lock: ^c.q ⊙ ^c.~q ≡ q (sheds the label, Ex. 8) *)
  assert_equal d
    (Semantics.meet_normalize
       (Meet (Channeled ("^c", d), Channeled ("^c", Residue d))));
  (* classical key-lock keeps the label (Ex. 21): ^c.(^c.i ⊙ ~i) ≡ ^c.i *)
  assert_equal
    (Channeled ("^c", Named "i"))
    (Semantics.meet_normalize
       (Channeled ("^c", Meet (Channeled ("^c", Named "i"), Residue (Named "i")))));
  (* classical cancellation: i ⊙ i ≡ ◦ *)
  let p = Channeled ("^c", Named "m") in
  assert_equal Blank (Semantics.meet_normalize (Meet (p, p)))

(* ---- Phase 3: circuit IR, scheduling, deadlock detection, passes --------- *)

let qasm_of_string s =
  match parse_membranes_from_string s with
  | Ok cfg -> Compile.compile_to_qasm cfg
  | Error _ -> assert_failure ("parse failed: " ^ s)

(* The receiver membrane may come FIRST: the scheduler blocks its waits until
   the sender's instructions are emitted (old "sender first" hack removed). *)
let receiver_first_teleport = "{ nu c.c?u.a?z.u<-z., o }, { nu c.c<-d.c->x.a!x., d, o }"

let test_compile_receiver_first _ =
  let q = qasm_of_string receiver_first_teleport in
  (* layout: Bob o=q0, Alice d=q1 o=q2; Bob (first creator) drives the Bell *)
  assert_bool "Bell pair q0-q2" (contains_sub q "cx q[0], q[2];");
  assert_bool "encode CNOT" (contains_sub q "cx q[1], q[2];");
  assert_bool "X correction on Bob" (contains_sub q "if(m0==1) x q[0];");
  assert_bool "Z correction on Bob" (contains_sub q "if(m1==1) z q[0];");
  assert_bool "no deadlock" (not (contains_sub q "deadlock"))

(* ... and the scheduled circuit still teleports: message starts on q[1]
   (Alice's d), must arrive on q[0] (Bob) in every branch. *)
let test_simulate_receiver_first _ =
  let qasm = qasm_of_string receiver_first_teleport in
  let alpha = 0.6 and beta = 0.8 in
  let branches =
    Sim.run ~init:(1, ({ Complex.re = alpha; im = 0.0 }, { Complex.re = beta; im = 0.0 })) qasm
  in
  assert_equal ~printer:string_of_int 4 (List.length branches);
  List.iter
    (fun br ->
      (* m0 measured q2, m1 measured q1; Bob is q0 (bit value 1) *)
      let bit r = match List.assoc_opt r br.Sim.cregs with Some v -> v | None -> 0 in
      let base = (2 * bit "m1") + (4 * bit "m0") in
      let a0 = br.Sim.amp.(base) and a1 = br.Sim.amp.(base + 1) in
      let p = Complex.norm2 a0 +. Complex.norm2 a1 in
      assert_bool "branch probability 1/4" (Float.abs (p -. 0.25) < 1e-9);
      let cross =
        Complex.sub
          (Complex.mul a0 { Complex.re = beta; im = 0.0 })
          (Complex.mul a1 { Complex.re = alpha; im = 0.0 })
      in
      assert_bool "Bob's qubit carries the message" (Complex.norm cross < 1e-9))
    branches

(* Circular classical dependency is reported, not silently miscompiled. *)
let test_compile_deadlock_detected _ =
  let q = qasm_of_string "{ a?x.b!x. }, { b?y.a!y. }" in
  assert_bool "deadlock reported"
    (contains_sub q "classical synchronization deadlock")

(* Unused resources do not waste qubits: the register is compacted. *)
let test_compile_dead_qubit_elimination _ =
  let q = qasm_of_string "{ nu c., o, zz }, { nu c., o }" in
  assert_bool "register compacted to 2" (contains_sub q "qreg q[2];");
  assert_bool "Bell pair remapped to q[0],q[1]" (contains_sub q "cx q[0], q[1];");
  assert_bool "no gate on the dead slot" (not (contains_sub q "cx q[0], q[2]"))

(* Peephole pass: adjacent self-inverse gate pairs cancel. *)
let test_ir_gate_cancellation _ =
  assert_equal [ (0, Ir.X 1) ]
    (Ir.cancel_gates [ (0, Ir.H 0); (0, Ir.H 0); (0, Ir.X 1) ]);
  assert_equal []
    (Ir.cancel_gates
       [ (0, Ir.CX (0, 1)); (1, Ir.CX (0, 1)); (0, Ir.Z 2); (0, Ir.Z 2) ]);
  (* different qubits do not cancel *)
  assert_equal [ (0, Ir.H 0); (0, Ir.H 1) ]
    (Ir.cancel_gates [ (0, Ir.H 0); (0, Ir.H 1) ])

(* Phase 2 closure: exact (unbounded) traces for the replication-free
   fragment agree with the bounded approximation, and repl is rejected. *)
let test_traces_exact _ =
  let parse s =
    match parse_membranes_from_string s with
    | Ok c -> c
    | Error _ -> assert_failure ("parse failed: " ^ s)
  in
  let choice = parse "{ a!x. + b!y. }, { a?u. }, { b?v. }" in
  assert_equal ~printer:(String.concat " | ")
    (Semantics.traces ~depth:8 choice)
    (Semantics.traces_exact choice);
  assert_bool "exact refinement reflexive" (Semantics.trace_refines_exact choice choice);
  assert_raises (Invalid_argument "traces_exact: configuration uses repl; use traces ~depth")
    (fun () -> Semantics.traces_exact (parse "{ repl nu c., o }"))

(* ---- Phase 4: backends — OpenQASM 3 and the concurrent target (b) -------- *)

let circuit_of_string s =
  match parse_membranes_from_string s with
  | Ok cfg -> Compile.lower cfg
  | Error _ -> assert_failure ("parse failed: " ^ s)

let circuit_of_file path = circuit_of_string (read_file path)

let test_qasm3_teleport _ =
  let q = Ir.to_qasm3 (circuit_of_file "examples/qam/teleport.qam") in
  assert_bool "QASM3 header" (String.starts_with ~prefix:"OPENQASM 3.0;" q);
  assert_bool "qubit decl" (contains_sub q "qubit[3] q;");
  assert_bool "bit decl" (contains_sub q "bit m0;");
  assert_bool "measure assignment" (contains_sub q "m0 = measure q[1];");
  assert_bool "real if statement" (contains_sub q "if (m0 == 1) { x q[2]; }");
  assert_bool "Z correction" (contains_sub q "if (m1 == 1) { z q[2]; }")

(* superdense's classical message becomes a genuine runtime input parameter *)
let test_qasm3_input_bit _ =
  let q = Ir.to_qasm3 (circuit_of_file "examples/qam/superdense.qam") in
  assert_bool "input bit" (contains_sub q "input bit i;");
  assert_bool "input-controlled X" (contains_sub q "if (i == 1) { x q[0]; }")

let test_concurrent_emit _ =
  let t = Ir.to_concurrent (circuit_of_file "examples/qam/teleport.qam") in
  assert_bool "shared state" (contains_sub t "shared qubit[3] q;");
  assert_bool "process 0 region" (contains_sub t "process 0 owns q[0,1] {");
  assert_bool "process 1 region" (contains_sub t "process 1 owns q[2] {");
  assert_bool "send sync" (contains_sub t "send a;");
  assert_bool "wait sync" (contains_sub t "wait c;");
  assert_bool "corrections local to Bob" (contains_sub t "if (m0 == 1) { x q[2]; }")

(* Locality (paper Thm. 7 territory): the ONLY instruction touching another
   membrane's region is Cohere's collaborative Bell-pair CX. *)
let test_concurrent_locality _ =
  match Ir.cross_region (circuit_of_file "examples/qam/teleport.qam") with
  | [ (0, Ir.CX (1, 2)) ] -> ()
  | l -> assert_failure (Printf.sprintf "expected only the Bell CX, got %d cross-region ops" (List.length l))

(* The concurrent runtime teleports: same physics assertions as the flat
   backend, but executed per-process with blocking send/wait.
   [meas] maps each measurement creg to the qubit it measured. *)
let assert_teleports ~bob ~meas branches =
  let alpha = 0.6 and beta = 0.8 in
  assert_equal ~printer:string_of_int 4 (List.length branches);
  List.iter
    (fun br ->
      let bit r = match List.assoc_opt r br.Sim.cregs with Some v -> v | None -> 0 in
      let base =
        List.fold_left
          (fun acc (r, q) -> if bit r = 1 then acc lor (1 lsl q) else acc)
          0 meas
      in
      let a0 = br.Sim.amp.(base) and a1 = br.Sim.amp.(base lor (1 lsl bob)) in
      let p = Complex.norm2 a0 +. Complex.norm2 a1 in
      assert_bool "branch probability 1/4" (Float.abs (p -. 0.25) < 1e-9);
      let cross =
        Complex.sub
          (Complex.mul a0 { Complex.re = beta; im = 0.0 })
          (Complex.mul a1 { Complex.re = alpha; im = 0.0 })
      in
      assert_bool "Bob's qubit carries the message" (Complex.norm cross < 1e-9))
    branches

let test_concurrent_sim_teleport _ =
  let c = circuit_of_file "examples/qam/teleport.qam" in
  let branches =
    Sim.run_ir ~init:(0, ({ Complex.re = 0.6; im = 0.0 }, { Complex.re = 0.8; im = 0.0 })) c
  in
  (* msg q0; m0 measured q1, m1 measured q0; Bob q2 *)
  assert_teleports ~bob:2 ~meas:[ ("m0", 1); ("m1", 0) ] branches

(* receiver membrane first: the runtime's blocking waits handle it *)
let test_concurrent_sim_receiver_first _ =
  let c = circuit_of_string receiver_first_teleport in
  let branches =
    Sim.run_ir ~init:(1, ({ Complex.re = 0.6; im = 0.0 }, { Complex.re = 0.8; im = 0.0 })) c
  in
  (* msg q1; m0 measured q2, m1 measured q1; Bob q0 *)
  assert_teleports ~bob:0 ~meas:[ ("m0", 2); ("m1", 1) ] branches

let test_concurrent_deadlock_raises _ =
  let c = circuit_of_string "{ a?x.b!x. }, { b?y.a!y. }" in
  assert_raises (Failure "concurrent execution deadlock on channel(s): a, b")
    (fun () -> Sim.run_ir c)

(* ---- Phase 6: entanglement swap + the extended QAM (network.ml) ---------- *)

let qes_src =
  "{ nu c., o }, { nu c.nu d.d<-c.d->y.a1!y., o, o }, { nu d.d?w.a1?z.w<-z., o }"

let qes_cfg () =
  match parse_membranes_from_string qes_src with
  | Ok c -> c
  | Error _ -> assert_failure "QES parse failed"

(* Quantum entanglement swap (paper Examples 9-10): the router hands its
   c-party to Bob through channel d; terminally Ann and Bob share channel c. *)
let test_machine_entanglement_swap _ =
  let terms = Semantics.reachable_terminals ~depth:18 (qes_cfg ()) in
  assert_bool "QES reaches { c.o }, { c.o }"
    (List.exists
       (fun (t, _) -> Semantics.string_of_config t = "{ c.o }, { c.o }")
       terms)

let qes_net =
  {
    Network.locs = [| "Ann"; "R"; "Bob" |];
    theta = [ (("Ann", "R"), 0.5); (("R", "Bob"), 0.5) ];
    intent = [ ("c", ("Ann", "Bob")); ("d", ("R", "Bob")) ];
  }

(* Extended QAM (Sec. 4.1): the QES run raises exactly the paper's labels —
   0.5(Ann,R) for channel c, 0.5(R,Bob) for d, then rate-1 for the transfer:
   total success rate 0.5 * 0.5 * 1 = 0.25. *)
let test_network_qes_trace _ =
  let ts = Network.ntraces ~policy:Network.QPass ~depth:18 qes_net (qes_cfg ()) in
  let rendered =
    List.map (fun t -> String.concat ";" (List.map Network.string_of_nlabel t)) ts
  in
  assert_bool "paper's likeliness trace present"
    (List.mem "0.50(Ann,R);0.50(R,Bob);1.00(R,Bob)" rendered)

(* rule Chan filters: without a theta edge the channel cannot be built *)
let test_network_blocked_edge _ =
  let net = { qes_net with Network.theta = [ (("Ann", "R"), 0.5) ] } in
  let ts = Network.ntraces ~policy:Network.AnyEdge ~depth:18 net (qes_cfg ()) in
  List.iter
    (fun t ->
      List.iter
        (function
          | Network.NRate (_, g, h) when (g, h) = ("R", "Bob") || (g, h) = ("Bob", "R") ->
              assert_failure "channel d built despite missing theta edge"
          | _ -> ())
        t)
    ts

(* Fig. 9 graph with Example 13's rates: QPass commits to the shortest path
   Ann-Ra-Bob; QCast to the SUM-maximizing path Ann-Rb-Rc-Bob. *)
let fig9 rates =
  {
    Network.locs = [||];
    theta =
      [ (("Ann", "Cat"), List.nth rates 0); (("Ann", "Ra"), List.nth rates 1);
        (("Ann", "Rb"), List.nth rates 2); (("Cat", "Ra"), List.nth rates 3);
        (("Ra", "Dan"), List.nth rates 4); (("Ra", "Bob"), List.nth rates 5);
        (("Dan", "Bob"), List.nth rates 6); (("Rb", "Rc"), List.nth rates 7);
        (("Rc", "Bob"), List.nth rates 8) ];
    intent = [ ("c", ("Ann", "Bob")) ];
  }

let ex13_net = fig9 [ 0.3; 0.5; 0.3; 0.3; 0.3; 0.3; 0.3; 0.5; 0.5 ]

let test_qpass_qcast_paths _ =
  assert_equal ~printer:(String.concat "-")
    [ "Ann"; "Ra"; "Bob" ]
    (match Network.chosen_path Network.QPass ex13_net ("Ann", "Bob") with
    | Some p -> p
    | None -> assert_failure "no QPass path");
  assert_equal ~printer:(String.concat "-")
    [ "Ann"; "Rb"; "Rc"; "Bob" ]
    (match Network.chosen_path Network.QCast ex13_net ("Ann", "Bob") with
    | Some p -> p
    | None -> assert_failure "no QCast path")

(* Example 12 vs 13: a channel creation on edge (Ann,Ra) is validated under
   QPass (edge on the shortest path) but invalidated under QCast (the
   max-sum path avoids Ra entirely). *)
let test_qpass_vs_qcast_filtering _ =
  let cfg =
    match parse_membranes_from_string "{ nu c., o }, { nu c., o }" with
    | Ok c -> c
    | Error _ -> assert_failure "parse failed"
  in
  let net = { ex13_net with Network.locs = [| "Ann"; "Ra" |] } in
  let render p =
    Network.ntraces ~policy:p ~depth:4 net cfg
    |> List.map (fun t -> String.concat ";" (List.map Network.string_of_nlabel t))
  in
  assert_bool "QPass validates (Ann,Ra)" (List.mem "0.50(Ann,Ra)" (render Network.QPass));
  assert_bool "QCast invalidates (Ann,Ra)"
    (not (List.exists (fun s -> s <> "") (render Network.QCast)))

(* Defs. 17-19: likeliness order and bounded LQTR *)
let test_likely_refinement _ =
  assert_bool "lower rate refines higher"
    (Network.likely_leq
       [ Network.NRate (0.3, "A", "B") ]
       [ Network.NRate (0.5, "A", "B") ]);
  assert_bool "higher rate does not refine lower"
    (not
       (Network.likely_leq
          [ Network.NRate (0.5, "A", "B") ]
          [ Network.NRate (0.3, "A", "B") ]));
  (* Theorem 20 flavor: on the line network both policies choose the same
     paths, so the QPass system refines the QCast system *)
  assert_bool "QPass refines QCast on the QES net"
    (Network.likely_refines ~pol1:Network.QPass ~pol2:Network.QCast ~depth:18 qes_net
       (qes_cfg ()))

let test_chem_dsl_parse _ =
  let dsl =
    String.concat "\n"
      [
        "name H2";
        "qubits 2";
        "ref -1.137";
        "term -1.0523732 I0";
        "term 0.3979374 Z0";
        "term -0.3979374 Z1";
      ]
  in
  match parse_problem dsl with
  | Ok p ->
      assert_equal "H2" p.name;
      assert_equal 2 p.n_qubits;
      assert_bool "terms parsed" (List.length p.terms >= 3)
  | Error msg -> assert_failure ("parse_problem failed: " ^ msg)

let test_vqe_loop_decreases_energy _ =
  let cfg = { default_config with steps = 40; learning_rate = 0.06 } in
  let r = run_vqe ~cfg builtin_h2 in
  match r.history with
  | [] | [ _ ] -> assert_failure "history too short"
  | (_, e0) :: _ ->
      assert_bool
        "final energy should not be worse than initial"
        (r.final_energy <= e0 +. 1e-6)

let test_openqasm_export _ =
  let r = run_vqe builtin_h2 in
  assert_bool "qasm header" (String.starts_with ~prefix:"OPENQASM 2.0;" r.qasm);
  assert_bool "qreg present"
    (String.contains r.qasm 'q' && String.contains r.qasm '[')

let test_benchmark_suite_h2_lih _ =
  let rows = benchmark_suite () in
  let names = List.map (fun (p, _) -> p.name) rows in
  assert_bool "contains H2" (List.mem "H2" names);
  assert_bool "contains LiH" (List.mem "LiH" names)

let suite =
  "rewrite_tests"
  >::: [
         "test_check_resource_equivalence" >:: test_check_resource_equivalence;
         "test_check_process_equivalence" >:: test_check_process_equivalence;
         "test_check_molecule_equivalence" >:: test_check_molecule_equivalence;
         "test_check_membrane_equivalence" >:: test_check_membrane_equivalence;
         "test_parser_choice_and_repl" >:: test_parser_choice_and_repl;
         "test_compile_bell_pair" >:: test_compile_bell_pair;
         "test_compile_missing_blank" >:: test_compile_missing_blank;
         "test_compile_bit_commitment" >:: test_compile_bit_commitment;
         "test_compile_teleportation" >:: test_compile_teleportation;
         "test_compile_superdense" >:: test_compile_superdense;
         "test_bell_simulates" >:: test_bell_simulates;
         "test_teleport_simulates" >:: test_teleport_simulates;
         "test_parser_sequencing" >:: test_parser_sequencing;
         "test_typecheck_protocols" >:: test_typecheck_protocols;
         "test_typecheck_omega" >:: test_typecheck_omega;
         "test_typecheck_no_cloning" >:: test_typecheck_no_cloning;
         "test_typecheck_channel_arity" >:: test_typecheck_channel_arity;
         "test_typecheck_kind_conflict" >:: test_typecheck_kind_conflict;
         "test_typecheck_missing_blank" >:: test_typecheck_missing_blank;
         "test_typecheck_double_decode" >:: test_typecheck_double_decode;
         "test_typecheck_residue_as_channel" >:: test_typecheck_residue_as_channel;
         "test_typecheck_accepts_valid" >:: test_typecheck_accepts_valid;
         "test_typecheck_one_payload" >:: test_typecheck_one_payload;
         "test_width2_teleport" >:: test_width2_teleport;
         "test_definitions_compile_equal" >:: test_definitions_compile_equal;
         "test_definitions_in_continuation" >:: test_definitions_in_continuation;
         "test_definitions_undefined_ref" >:: test_definitions_undefined_ref;
         "test_machine_teleportation" >:: test_machine_teleportation;
         "test_machine_superdense" >:: test_machine_superdense;
         "test_machine_bitcommit" >:: test_machine_bitcommit;
         "test_machine_choice_both_branches" >:: test_machine_choice_both_branches;
         "test_machine_trace_refinement" >:: test_machine_trace_refinement;
         "test_machine_meet_algebra" >:: test_machine_meet_algebra;
         "test_compile_receiver_first" >:: test_compile_receiver_first;
         "test_simulate_receiver_first" >:: test_simulate_receiver_first;
         "test_compile_deadlock_detected" >:: test_compile_deadlock_detected;
         "test_compile_dead_qubit_elimination" >:: test_compile_dead_qubit_elimination;
         "test_ir_gate_cancellation" >:: test_ir_gate_cancellation;
         "test_traces_exact" >:: test_traces_exact;
         "test_qasm3_teleport" >:: test_qasm3_teleport;
         "test_qasm3_input_bit" >:: test_qasm3_input_bit;
         "test_concurrent_emit" >:: test_concurrent_emit;
         "test_concurrent_locality" >:: test_concurrent_locality;
         "test_concurrent_sim_teleport" >:: test_concurrent_sim_teleport;
         "test_concurrent_sim_receiver_first" >:: test_concurrent_sim_receiver_first;
         "test_concurrent_deadlock_raises" >:: test_concurrent_deadlock_raises;
         "test_machine_entanglement_swap" >:: test_machine_entanglement_swap;
         "test_network_qes_trace" >:: test_network_qes_trace;
         "test_network_blocked_edge" >:: test_network_blocked_edge;
         "test_qpass_qcast_paths" >:: test_qpass_qcast_paths;
         "test_qpass_vs_qcast_filtering" >:: test_qpass_vs_qcast_filtering;
         "test_likely_refinement" >:: test_likely_refinement;
         "test_chem_dsl_parse" >:: test_chem_dsl_parse;
         "test_vqe_loop_decreases_energy" >:: test_vqe_loop_decreases_energy;
         "test_openqasm_export" >:: test_openqasm_export;
         "test_benchmark_suite_h2_lih" >:: test_benchmark_suite_h2_lih;
       ]

let () = run_test_tt_main suite
