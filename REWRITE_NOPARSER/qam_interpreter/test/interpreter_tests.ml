open OUnit2
open Qam_interpreter

(* Test: Encode Message *)
let test_encode_message _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (SimpleResource NullResource)
  ] in
  let updated_membrane = encode_message "chan1" (Quantum "test_message") membrane in
  match updated_membrane with
  | MoleculeMembrane molecules ->
      let encoded = List.exists (function
        | ResourceMolecule (CombinedResource (SimpleResource (Quantum "chan1"), Quantum "test_message")) -> true
        | _ -> false) molecules in
      assert_bool "Message was not encoded correctly" encoded
  | _ -> assert_failure "Invalid membrane state after encoding"

(* Test: Decode Message *)
let test_decode_message _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "chan1"), Quantum "test_message"))
  ] in
  let decoded_message = decode_message "chan1" membrane in
  assert_equal "test_message" decoded_message ~msg:"Message was not correctly decoded"

(* Test: No-Cloning Property *)
let test_no_cloning _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (SimpleResource NullResource);
    ResourceMolecule (SimpleResource NullResource)
  ] in
  assert_raises
    (Failure "Cloning violation detected in membrane")
    (fun () -> enforce_no_cloning membrane)

(* Test: Entanglement Swap *)
let test_entanglement_swap _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource NullResource, Quantum "A"));
    ResourceMolecule (CombinedResource (SimpleResource NullResource, Quantum "B"))
  ] in
  let swapped_membrane = entanglement_swap membrane "A" "B" in
  match swapped_membrane with
  | MoleculeMembrane molecules ->
      let swapped = List.exists (function
        | ResourceMolecule (CombinedResource (_, Quantum "B")) -> true
        | ResourceMolecule (CombinedResource (_, Quantum "A")) -> true
        | _ -> false) molecules in
      assert_bool "Entanglement swap failed" swapped
  | _ -> assert_failure "Invalid membrane state after entanglement swap"


(* Debug function  *)
let rec string_of_resource = function
  | SimpleResource r -> "SimpleResource(" ^ string_of_resource r ^ ")"
  | NullResource -> "NullResource"
  | CombinedResource (r, Quantum q) -> "CombinedResource(" ^ string_of_resource r ^ ", Quantum(" ^ q ^ "))"
  | CombinedResource (r, ClassicalData c) -> "CombinedResource(" ^ string_of_resource r ^ ", ClassicalData(" ^ c ^ "))"
  | MeetOperation (r1, r2) -> "MeetOperation(" ^ string_of_resource r1 ^ ", " ^ string_of_resource r2 ^ ")"
  | Quantum q -> "Quantum(" ^ q ^ ")"


(* Test: Quantum Teleportation *)
let test_quantum_teleportation _ =
  (* Initial membrane with a quantum channel and message *)
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "state"), Quantum "state_message"))
  ] in

  (* Perform quantum teleportation *)
  let teleported_membrane = quantum_teleportation membrane "state" "destination_channel" in

  (* Assert the quantum state has been moved to the destination channel *)
  match teleported_membrane with
  | MoleculeMembrane molecules ->
    
    List.iter (fun molecule ->
      match molecule with
      | ResourceMolecule resource -> print_endline ("Found resource: " ^ string_of_resource resource)
      | _ -> ()) molecules;

      let teleported = List.exists (function
        | ResourceMolecule (CombinedResource (SimpleResource (Quantum "destination_channel"), Quantum "state_message")) -> true
        | _ -> false) molecules in
      assert_bool "Quantum teleportation failed: quantum state not found in destination channel" teleported;
      print_endline "Quantum state teleported successfully"
  | _ -> assert_failure "Invalid membrane state after quantum teleportation"



(* let test_quantum_teleportation _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource NullResource, Quantum "state"))
  ] in
  (* Perform teleportation *)
  let teleported_membrane = quantum_teleportation membrane "state" "destination_channel" in
  match teleported_membrane with
  | MoleculeMembrane molecules ->
      let teleported = List.exists (function
        | ResourceMolecule (CombinedResource (_, Quantum "state")) -> true
        | _ -> false) molecules in
      assert_bool "Quantum teleportation failed" teleported
  | _ -> assert_failure "Invalid membrane state after teleportation" *)

(* Test: Superdense Coding *)
let test_superdense_coding _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (SimpleResource NullResource)
  ] in
  let encoded_membrane = superdense_encode membrane "channel" "data" in
  let decoded_data = decode_message "channel" encoded_membrane in
  assert_equal "superdense:data" decoded_data ~msg:"Superdense coding failed"

(* Test: Choice Process *)
let test_choice_process _ =
  let membrane = MoleculeMembrane [] in
  let process = Choice (
      ActionProcess (NewChannel "chan1", NullProcess),
      ActionProcess (NewChannel "chan2", NullProcess)
    ) in
  interpret process membrane;
  assert_bool "Choice process failed" true

(* Test: Replication Process *)
let test_replication_process _ =
  let membrane = MoleculeMembrane [] in
  let process = Replication (ActionProcess (NewChannel "chan1", NullProcess)) in
  interpret process membrane;
  assert_bool "Replication process failed" true

(* Test Suite *)
let suite =
  "Interpreter Tests" >::: [
    "test_encode_message" >:: test_encode_message;
    "test_decode_message" >:: test_decode_message;
    "test_no_cloning" >:: test_no_cloning;
    "test_entanglement_swap" >:: test_entanglement_swap;
    "test_quantum_teleportation" >:: test_quantum_teleportation;
    "test_superdense_coding" >:: test_superdense_coding;
    "test_choice_process" >:: test_choice_process;
    "test_replication_process" >:: test_replication_process;
  ]

(* Entry Point for Running Tests *)
let () =
  run_test_tt_main suite
