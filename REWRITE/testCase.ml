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

let test_check_resource_equivalence = 
    let r1 = SimpleResource (SimpleResource NullResource) in
    let r2 = SimpleResource (SimpleResource NullResource) in
    assert_equal true (check_resource_equivalence r1 r2);
  
    let r1 = SimpleResource (SimpleResource NullResource) in
    let r2 = SimpleResource NullResource in
    assert_equal false (check_resource_equivalence r1 r2);
  
    let r1 = CombinedResource (SimpleResource NullResource, Quantum "input") in
    let r2 = CombinedResource (SimpleResource NullResource, Quantum "input") in
    assert_equal true (check_resource_equivalence r1 r2);
  
    let r1 = CombinedResource (SimpleResource NullResource, Quantum "input") in
    let r2 = CombinedResource (SimpleResource NullResource, Quantum "input") in
    assert_equal false (check_resource_equivalence r1 r2);
  
    let r1 = MeetOperation (SimpleResource NullResource, SimpleResource NullResource) in
    let r2 = MeetOperation (SimpleResource NullResource, SimpleResource NullResource) in
    assert_equal true (check_resource_equivalence r1 r2);
  
    let r1 = MeetOperation (SimpleResource NullResource, SimpleResource NullResource) in
    let r2 = MeetOperation (SimpleResource NullResource, SimpleResource NullResource) in
    assert_equal false (check_resource_equivalence r1 r2)
  
let test_check_molecule_equivalence =
    let m1 = ProcessMolecule NullProcess in
    let m2 = ProcessMolecule NullProcess in
    assert_equal true (check_molecule_equivalence m1 m2);
  
    let m1 = ProcessMolecule NullProcess in
    let m2 = ProcessMolecule NullProcess in
    assert_equal false (check_molecule_equivalence m1 m2);
  
    let m1 = ResourceMolecule (SimpleResource NullResource) in
    let m2 = ResourceMolecule (SimpleResource NullResource) in
    assert_equal true (check_molecule_equivalence m1 m2);
  
    let m1 = ResourceMolecule (SimpleResource NullResource) in
    let m2 = ResourceMolecule (SimpleResource NullResource) in
    assert_equal false (check_molecule_equivalence m1 m2);
  
    let m1 = ResourceMolecule (SimpleResource NullResource) in
    let m2 = ProcessMolecule NullProcess in
    assert_equal false (check_molecule_equivalence m1 m2)

let suite =
  "TestCheckProcessEquivalence" >::: [
    "test_check_process_equivalence" >:: test_check_process_equivalence
  ]

let () =
  run_test_tt_main suite
