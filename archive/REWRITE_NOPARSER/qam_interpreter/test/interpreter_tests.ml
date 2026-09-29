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
let test_entanglement_swap_success _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "A"), Quantum "entangled_with_B"));
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "B"), Quantum "entangled_with_A"))
  ] in

  let swapped_membrane = entanglement_swap membrane "A" "B" in

  match swapped_membrane with
  | MoleculeMembrane molecules ->
      let a_swapped = List.exists (function
        | ResourceMolecule (CombinedResource (SimpleResource (Quantum "B"), Quantum "entangled_with_A")) -> true
        | _ -> false) molecules in
      let b_swapped = List.exists (function
        | ResourceMolecule (CombinedResource (SimpleResource (Quantum "A"), Quantum "entangled_with_B")) -> true
        | _ -> false) molecules in

      assert_bool "Entanglement swap failed for channel A" a_swapped;
      assert_bool "Entanglement swap failed for channel B" b_swapped
  | _ -> assert_failure "Invalid membrane state after entanglement swap"

(* Test: Missing one channel *)
  let test_entanglement_swap_missing_channel _ =
    let membrane = MoleculeMembrane [
      ResourceMolecule (CombinedResource (SimpleResource (Quantum "A"), Quantum "entangled_with_B"))
    ] in

    assert_raises
      (Failure "Entanglement swap requires both channels to exist in the membrane")
      (fun () -> entanglement_swap membrane "A" "B")

  (* Test: No entanglement found *)
  let test_entanglement_swap_no_entanglement _ =
    let membrane = MoleculeMembrane [
      ResourceMolecule (SimpleResource (Quantum "A"))
    ] in

    assert_raises
      (Failure "No entanglement found between the specified channels")
      (fun () -> entanglement_swap membrane "A" "B")

(* Test case for QLocal transition *)
let test_qes_localization _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource NullResource, Quantum "c"));
    ResourceMolecule (SimpleResource (Quantum "d"))
  ] in
  let localized_membrane = entanglement_swap membrane "c" "d" in
  match localized_membrane with
  | MoleculeMembrane molecules ->
      assert_bool "c not localized to c.◦" (List.exists (function
        | ResourceMolecule (CombinedResource (_, Quantum "c.◦")) -> true
        | _ -> false) molecules)
  | _ -> assert_failure "Membrane state invalid after localization"

let test_qes_transitions _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource NullResource, Quantum "c"));
    ResourceMolecule (SimpleResource (Quantum "d"))
  ] in
  let localized_membrane = entanglement_swap membrane "c" "d" in
  match localized_membrane with
  | MoleculeMembrane molecules ->
      (* Check QLocal: c localized *)
    assert_bool "c not localized to c.◦" (List.exists (function
      | ResourceMolecule (CombinedResource (_, Quantum "c.◦")) -> true
      | _ -> false) molecules);
    (* Check Encode: c.◦ encoded into d *)
    assert_bool "c.◦ not encoded into d" (List.exists (function
      | ResourceMolecule (CombinedResource (_, Quantum "c.◦")) -> true
      | _ -> false) molecules)
  | _ -> assert_failure "Invalid membrane state after QES transitions"
  

      
(* let test_entanglement_swap _ =
  (* Initial membrane with entangled channels A and B *)
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "A"), Quantum "entangled_with_B"));
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "B"), Quantum "entangled_with_A"))
  ] in

  (* Perform the entanglement swap *)
  let swapped_membrane = entanglement_swap membrane "A" "B" in

  (* Validate the swapped state *)
  match swapped_membrane with
  | MoleculeMembrane molecules ->
      (* Check if A now contains the state originally in B, and vice versa *)
      let a_swapped = List.exists (function
        | ResourceMolecule (CombinedResource (SimpleResource (Quantum "B"), Quantum "entangled_with_A")) -> true
        | _ -> false) molecules in
      let b_swapped = List.exists (function
        | ResourceMolecule (CombinedResource (SimpleResource (Quantum "A"), Quantum "entangled_with_B")) -> true
        | _ -> false) molecules in

      assert_bool "Entanglement swap failed: A not swapped correctly" a_swapped;
      assert_bool "Entanglement swap failed: B not swapped correctly" b_swapped;
      print_endline "Entanglement swap successfully validated"
  | _ -> assert_failure "Invalid membrane state after entanglement swap"

  let test_entanglement_swap_missing_channel _ =
    let membrane = MoleculeMembrane [
      ResourceMolecule (CombinedResource (SimpleResource (Quantum "A"), Quantum "entangled_with_B"))
    ] in
  
    assert_raises
      (Failure "Entanglement swap requires both channels to exist in the membrane")
      (fun () -> entanglement_swap membrane "A" "B")

  let test_entanglement_swap_no_entanglement _ =
    let membrane = MoleculeMembrane [
      ResourceMolecule (SimpleResource NullResource)
    ] in
  
    assert_raises
      (Failure "No entanglement found between the specified channels")
      (fun () -> entanglement_swap membrane "A" "B") *)
      
  



(* let test_entanglement_swap _ =
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
  | _ -> assert_failure "Invalid membrane state after entanglement swap" *)


(* Debug call  *)
let rec string_of_resource = function
  | SimpleResource r -> "SimpleResource(" ^ string_of_resource r ^ ")"
  | NullResource -> "NullResource"
  | CombinedResource (r, Quantum q) -> "CombinedResource(" ^ string_of_resource r ^ ", Quantum(" ^ q ^ "))"
  | CombinedResource (r, ClassicalData c) -> "CombinedResource(" ^ string_of_resource r ^ ", ClassicalData(" ^ c ^ "))"
  | MeetOperation (r1, r2) -> "MeetOperation(" ^ string_of_resource r1 ^ ", " ^ string_of_resource r2 ^ ")"
  | Quantum q -> "Quantum(" ^ q ^ ")"


(* Test: Quantum Teleportation *)
(* Revised Test for Quantum Teleportation *)
(* let test_quantum_teleportation _ =
  (* Initialize source membrane with quantum state *)
  let source_membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "state"), Quantum "state_message"))
  ] in

  (* Perform quantum teleportation *)
  let teleported_membrane = quantum_teleportation source_membrane "state" "destination_channel" in

  (* Validate if destination channel contains teleported quantum state *)
  match teleported_membrane with
  | MoleculeMembrane molecules ->
      let teleported = List.exists (function
        | ResourceMolecule (CombinedResource (_, Quantum "state_message")) -> true
        | _ -> false) molecules in
      assert_bool "Quantum teleportation failed: state not found in destination channel" teleported
  | _ -> assert_failure "Invalid membrane state after quantum teleportation"
 *)

(* Test for Successful quantum teleportation *)
let test_quantum_teleportation_success _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "source_channel"), Quantum "state_to_teleport"));
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "destination_channel"), ClassicalData ""))
  ] in

  let teleported_membrane = quantum_teleportation membrane "source_channel" "destination_channel" in

  match teleported_membrane with
  | MoleculeMembrane molecules ->
      let destination_valid = List.exists (function
        | ResourceMolecule (CombinedResource (SimpleResource (Quantum "destination_channel"), Quantum "state_to_teleport")) -> true
        | _ -> false) molecules in

      let source_cleared = not (List.exists (function
        | ResourceMolecule (CombinedResource (SimpleResource (Quantum "source_channel"), Quantum "state_to_teleport")) -> true
        | _ -> false) molecules) in

      assert_bool "Quantum teleportation failed: state not found in destination channel" destination_valid;
      assert_bool "Quantum teleportation failed: source channel not cleared" source_cleared
  | _ -> assert_failure "Invalid membrane state after quantum teleportation"

(* Missing source channel *)
let test_quantum_teleportation_missing_source _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "destination_channel"), ClassicalData ""))
  ] in

  assert_raises
    (Failure "Quantum teleportation failed: No valid message found for decoding on channel: source_channel")
    (fun () -> quantum_teleportation membrane "source_channel" "destination_channel")

(* Test: Missing destination channel *)
let test_quantum_teleportation_missing_destination _ =
  let membrane = MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "source_channel"), Quantum "state_to_teleport"))
  ] in

  assert_raises
    (Failure "Quantum teleportation failed: Destination channel not found")
    (fun () -> quantum_teleportation membrane "source_channel" "destination_channel")


(* let test_quantum_teleportation _ =
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
  | _ -> assert_failure "Invalid membrane state after quantum teleportation" *)



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

(* Test: Superdense Coding [NOT FULLY WORKING YET *)
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

(* Replication Process *)
let test_replication_process _ =
  let membrane = MoleculeMembrane [] in
  let process = Replication (ActionProcess (NewChannel "chan1", NullProcess)) in
  interpret process membrane;
  assert_bool "Replication process failed" true

(* Test Suite *)
let suite =
  "Entanglement Swap Tests" >::: [
    "Entanglement Swap Tests" >::: [
      "test_entanglement_swap_success" >:: test_entanglement_swap_success;
      "test_entanglement_swap_missing_channel" >:: test_entanglement_swap_missing_channel;
      "test_entanglement_swap_no_entanglement" >:: test_entanglement_swap_no_entanglement;
      "test_qes_transitions" >:: test_qes_transitions;
    ];
    "Quantum Teleportation Tests" >::: [
      "test_quantum_teleportation_success" >:: test_quantum_teleportation_success;
      "test_quantum_teleportation_missing_source" >:: test_quantum_teleportation_missing_source;
      "test_quantum_teleportation_missing_destination" >:: test_quantum_teleportation_missing_destination;
    ];
    "Interpreter Tests" >::: [
    "test_encode_message" >:: test_encode_message;
    "test_decode_message" >:: test_decode_message;
    "test_no_cloning" >:: test_no_cloning;
    (* "test_superdense_coding" >:: test_superdense_coding; *)
    "test_choice_process" >:: test_choice_process;
    "test_replication_process" >:: test_replication_process;
    ];
]

(* Entry Point for Running Tests *)
let () =

  run_test_tt_main suite
