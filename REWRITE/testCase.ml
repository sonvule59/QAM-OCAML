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
         "test_chem_dsl_parse" >:: test_chem_dsl_parse;
         "test_vqe_loop_decreases_energy" >:: test_vqe_loop_decreases_energy;
         "test_openqasm_export" >:: test_openqasm_export;
         "test_benchmark_suite_h2_lih" >:: test_benchmark_suite_h2_lih;
       ]

let () = run_test_tt_main suite
