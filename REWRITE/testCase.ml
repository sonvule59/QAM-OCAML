open OUnit2
open Ast
open Parser
open Lexer
open CheckEquivalence

let test_check_process_equivalence _ =
  let p1 = NullProcess in
  let p2 = NullProcess in
  assert_equal true (check_process_equivalence p1 p2);

  let p1 = ActionProcess ("a", NullProcess) in
  let p2 = ActionProcess ("a", NullProcess) in
  assert_equal true (check_process_equivalence p1 p2);

  let p1 = ActionProcess ("a", NullProcess) in
  let p2 = ActionProcess ("b", NullProcess) in
  assert_equal false (check_process_equivalence p1 p2);

  let p1 = Choice (NullProcess, ActionProcess ("a", NullProcess)) in
  let p2 = Choice (NullProcess, ActionProcess ("a", NullProcess)) in
  assert_equal true (check_process_equivalence p1 p2);

  let p1 = Replication (ActionProcess ("a", NullProcess)) in
  let p2 = Replication (ActionProcess ("a", NullProcess)) in
  assert_equal true (check_process_equivalence p1 p2);

  let p1 = Replication (ActionProcess ("a", NullProcess)) in
  let p2 = Replication (ActionProcess ("b", NullProcess)) in
  assert_equal false (check_process_equivalence p1 p2)

let suite =
  "TestCheckProcessEquivalence" >::: [
    "test_check_process_equivalence" >:: test_check_process_equivalence
  ]

let () =
  run_test_tt_main suite