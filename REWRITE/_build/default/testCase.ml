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
  let p1 = ActionProcess (Send "a!x", NullProcess) in
  let p2 = ActionProcess (Send "a!x", NullProcess) in
  let p3 = ActionProcess (Receive "a?x", NullProcess) in
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
        ProcessMolecule (ActionProcess (Send "a!x", NullProcess));
        ResourceMolecule (SimpleResource "a");
      ]
  in
  let same =
    MoleculeMembrane
      [
        ProcessMolecule (ActionProcess (Send "a!x", NullProcess));
        ResourceMolecule (SimpleResource "a");
      ]
  in
  let different =
    MoleculeMembrane
      [
        ProcessMolecule (ActionProcess (Receive "a?x", NullProcess));
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
         "test_chem_dsl_parse" >:: test_chem_dsl_parse;
         "test_vqe_loop_decreases_energy" >:: test_vqe_loop_decreases_energy;
         "test_openqasm_export" >:: test_openqasm_export;
         "test_benchmark_suite_h2_lih" >:: test_benchmark_suite_h2_lih;
       ]

let () = run_test_tt_main suite
