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
  let p1 = ActionProcess (Send { chan = "a"; arg = "x" }, NullProcess) in
  let p2 = ActionProcess (Send { chan = "a"; arg = "x" }, NullProcess) in
  let p3 = ActionProcess (Receive { chan = "a"; arg = "x" }, NullProcess) in
  assert_bool "same process" (check_process_equivalence p1 p2);
  assert_bool "different process action" (not (check_process_equivalence p1 p3))

let test_check_molecule_equivalence _ =
  let m1 = ProcessMolecule (ActionProcess (LeftCombine "a<-k", NullProcess)) in
  let m2 = ProcessMolecule (ActionProcess (LeftCombine "a<-k", NullProcess)) in
  let m3 = ResourceMolecule (SimpleResource "a") in
  assert_bool "same molecule" (check_molecule_equivalence m1 m2);
  assert_bool "different molecule kinds" (not (check_molecule_equivalence m1 m3))

let test_check_membrane_equivalence _ =
  let left =
    MoleculeMembrane
      [
        ProcessMolecule (ActionProcess (Send { chan = "a"; arg = "x" }, NullProcess));
        ResourceMolecule (SimpleResource "a");
      ]
  in
  let same =
    MoleculeMembrane
      [
        ProcessMolecule (ActionProcess (Send { chan = "a"; arg = "x" }, NullProcess));
        ResourceMolecule (SimpleResource "a");
      ]
  in
  let different =
    MoleculeMembrane
      [
        ProcessMolecule (ActionProcess (Receive { chan = "a"; arg = "x" }, NullProcess));
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

(* Reduction regression: normalize each example file to its exact normal form.
   Expected forms are documented in examples/qam/README.md. *)
let read_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let normal_form_of_file path =
  match parse_membranes_from_string (read_file path) with
  | Ok [ m ] -> string_of_membrane (normalize m)
  | Ok _ -> assert_failure ("expected exactly one membrane in " ^ path)
  | Error _ -> assert_failure ("parse failed for " ^ path)

let test_encode_normal_form _ =
  assert_equal ~printer:(fun s -> s) "{ 0, a & a }"
    (normal_form_of_file "examples/qam/encode.qam")

let test_cohere_normal_form _ =
  assert_equal ~printer:(fun s -> s) "{ 0, 0 }"
    (normal_form_of_file "examples/qam/cohere.qam")

let test_decode_normal_form _ =
  assert_equal ~printer:(fun s -> s) "{ 0, 0 }"
    (normal_form_of_file "examples/qam/decode.qam")

(* Choice and Replication now reduce (previously fixpoints, per audit). *)
let reduces s =
  match parse_membranes_from_string s with
  | Ok [ m ] -> normalize m <> m
  | _ -> assert_failure ("parse failed for " ^ s)

let test_choice_reduces _ =
  assert_bool "choice should reduce" (reduces "{ a!x. + b?y., o }")

let test_replication_reduces _ =
  assert_bool "replication should reduce" (reduces "{ repl nu c., o }")

(* ---- Phase 2: equivalence policy (normalize -> canonicalize -> structural) ----
   These exercise the real pipeline via check_equivalence_between_membranes. *)
let parse_one s =
  match parse_membranes_from_string s with
  | Ok [ m ] -> m
  | _ -> assert_failure ("parse failed for " ^ s)

let is_equiv a b =
  match check_equivalence_between_membranes (parse_one a) (parse_one b) with
  | Equivalent -> true
  | NotEquivalent _ -> false

(* Positive: must be Equivalent *)
let test_equiv_soup_permutation _ =
  assert_bool "soup permutation" (is_equiv "{ 0, a & a }" "{ a & a, 0 }")

let test_equiv_meet_commute _ =
  assert_bool "meet commutativity" (is_equiv "{ 0, a & b }" "{ 0, b & a }")

let test_equiv_reduce_then_equal _ =
  assert_bool "cohere -> {0,0}" (is_equiv "{ nu c., c!x. }" "{ 0, 0 }");
  assert_bool "decode -> {0,0}" (is_equiv "{ c->x., c?y. }" "{ 0, 0 }");
  assert_bool "encode -> {0, a & a}" (is_equiv "{ a<-k., a }" "{ 0, a & a }")

let test_equiv_reflexive_nf _ =
  assert_bool "reflexive NF" (is_equiv "{ 0, 0 }" "{ 0, 0 }")

let test_equiv_airlock_same _ =
  assert_bool "identical airlocks"
    (is_equiv "|[ { nu z., o }, phi, { c?d. } ]|"
       "|[ { nu z., o }, phi, { c?d. } ]|")

(* Negative: must be NotEquivalent *)
let test_not_equiv_distinct_nf _ =
  assert_bool "distinct NFs" (not (is_equiv "{ 0, a & a }" "{ 0, 0 }"))

(* Documented MVP limit: Choice commits left, so p + q and q + p have
   different normal forms and are NOT claimed equivalent. *)
let test_not_equiv_choice_asymmetry _ =
  assert_bool "choice asymmetry"
    (not (is_equiv "{ a!x. + b?y., o }" "{ b?y. + a!x., o }"))

let test_not_equiv_airlock_diff _ =
  assert_bool "different airlock boundary"
    (not
       (is_equiv "|[ { nu z., o }, phi, { c?d. } ]|"
          "|[ { nu z., o }, psi, { c?d. } ]|"))

(* Replication policy: fuel-bounded normalize + canon (no true fixpoint). *)
let test_equiv_repl_identical _ =
  assert_bool "identical repl terms stay equivalent (same fuel)"
    (is_equiv "{ repl nu c., o }" "{ repl nu c., o }")

(* A repl term is NOT claimed equivalent to its one-step unfold: after the same
   fuel budget the two fuel-bounded approximations differ. This asserts the
   honest outcome, not a pretended replication normal form. *)
let test_not_equiv_repl_vs_unfold _ =
  assert_bool "repl not equivalent to its one-step unfold"
    (not (is_equiv "{ repl nu c., o }" "{ nu c., repl nu c., o }"))

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
      ~init0:({ Complex.re = alpha; im = 0.0 }, { Complex.re = beta; im = 0.0 })
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
         "test_encode_normal_form" >:: test_encode_normal_form;
         "test_cohere_normal_form" >:: test_cohere_normal_form;
         "test_decode_normal_form" >:: test_decode_normal_form;
         "test_choice_reduces" >:: test_choice_reduces;
         "test_replication_reduces" >:: test_replication_reduces;
         "test_equiv_soup_permutation" >:: test_equiv_soup_permutation;
         "test_equiv_meet_commute" >:: test_equiv_meet_commute;
         "test_equiv_reduce_then_equal" >:: test_equiv_reduce_then_equal;
         "test_equiv_reflexive_nf" >:: test_equiv_reflexive_nf;
         "test_equiv_airlock_same" >:: test_equiv_airlock_same;
         "test_not_equiv_distinct_nf" >:: test_not_equiv_distinct_nf;
         "test_not_equiv_choice_asymmetry" >:: test_not_equiv_choice_asymmetry;
         "test_not_equiv_airlock_diff" >:: test_not_equiv_airlock_diff;
         "test_equiv_repl_identical" >:: test_equiv_repl_identical;
         "test_not_equiv_repl_vs_unfold" >:: test_not_equiv_repl_vs_unfold;
         "test_compile_bell_pair" >:: test_compile_bell_pair;
         "test_compile_missing_blank" >:: test_compile_missing_blank;
         "test_compile_bit_commitment" >:: test_compile_bit_commitment;
         "test_compile_teleportation" >:: test_compile_teleportation;
         "test_compile_superdense" >:: test_compile_superdense;
         "test_bell_simulates" >:: test_bell_simulates;
         "test_teleport_simulates" >:: test_teleport_simulates;
         "test_parser_sequencing" >:: test_parser_sequencing;
         "test_chem_dsl_parse" >:: test_chem_dsl_parse;
         "test_vqe_loop_decreases_energy" >:: test_vqe_loop_decreases_energy;
         "test_openqasm_export" >:: test_openqasm_export;
         "test_benchmark_suite_h2_lih" >:: test_benchmark_suite_h2_lih;
       ]

let () = run_test_tt_main suite
