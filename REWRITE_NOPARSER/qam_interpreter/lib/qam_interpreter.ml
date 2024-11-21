type input = string

type message =
  | Quantum of input
  | ClassicalData of input

type resource =
  | SimpleResource of resource
  | NullResource
  | CombinedResource of resource * message
  | MeetOperation of resource * resource
  | Quantum of string 

type action =
  | NewChannel of string
  | Send of string
  | Receive of string
  | Encode of string
  | Decode of string

type process =
  | NullProcess
  | ActionProcess of action * process
  | Choice of process * process
  | Replication of process

type molecule =
  | NullMolecule
  | ProcessMolecule of process
  | ResourceMolecule of resource

type membrane =
  | NullMembrane
  | MoleculeMembrane of molecule list
  | AirlockedMembrane of molecule list * resource * process

(* Logic *)

let meet_operation (r1 : resource) (r2 : resource) : resource =
  MeetOperation (r1, r2)

let print_membrane_state (m : membrane) =
  match m with
  | NullMembrane -> print_endline "NullMembrane"
  | MoleculeMembrane molecules ->
      List.iter
        (fun m ->
          match m with
          | NullMolecule -> print_endline "NullMolecule"
          | ProcessMolecule _ -> print_endline "ProcessMolecule"
          | ResourceMolecule _ -> print_endline "ResourceMolecule")
        molecules
  | AirlockedMembrane (_, _, _) -> print_endline "AirlockedMembrane"

let encode_message (channel : string) (message : message) (membrane : membrane) =
  match membrane with
  | MoleculeMembrane molecules ->
      let updated_molecules =
        List.map
          (function
            | ResourceMolecule (SimpleResource NullResource) ->
                (* Explicitly associate the channel with the message *)
              ResourceMolecule (CombinedResource (SimpleResource (Quantum channel), message))
            | other -> other)
                (* ResourceMolecule (CombinedResource (SimpleResource (SimpleResource (CombinedResource (NullResource, Quantum channel))), message))
            | other -> other) *)
          molecules
      in
      MoleculeMembrane updated_molecules
  | _ -> failwith "Encoding requires a molecule membrane"
  
(* let encode_message (channel : string) (message : message) (membrane : membrane) =
  match membrane with
  | MoleculeMembrane molecules ->
      let updated_molecules =
        List.map
          (function
            | ResourceMolecule (SimpleResource NullResource) when channel = "target_channel" ->
                ResourceMolecule (CombinedResource (SimpleResource NullResource, message))
            | other -> other)
          molecules
      in
      MoleculeMembrane updated_molecules
  | _ -> failwith "Encoding requires a molecule membrane" *)

let decode_message (channel : string) (membrane : membrane) =
  match membrane with
  | MoleculeMembrane molecules ->
      let decoded_message =
        List.fold_left
          (fun acc molecule ->
            match molecule with
            (* | ResourceMolecule (CombinedResource (SimpleResource (Quantum q), msg)) when channel = q ->
              (match msg with
              | Quantum q_msg -> Some q_msg
              | ClassicalData c_msg -> Some c_msg
              | _ -> acc) *)
          
            (* | ResourceMolecule (CombinedResource (_, Quantum q)) when channel = q -> Some q
            | ResourceMolecule (CombinedResource (_, ClassicalData c)) when channel = c -> Some c *)
            | ResourceMolecule (CombinedResource (SimpleResource (Quantum q), msg)) when channel = q ->
              Some msg
            | _ -> acc)
          None
          molecules
      in
      (match decoded_message with
      | Some (Quantum q_msg) -> q_msg
      | Some (ClassicalData c_msg) -> c_msg
      | None -> failwith ("No valid message found for decoding on channel: " ^ channel))
  | _ -> failwith "Decoding requires a molecule membrane"
  
  (* let decode_message (channel : string) (membrane : membrane) =
  match membrane with
  | MoleculeMembrane molecules ->
      let decoded_message =
        List.fold_left
          (fun acc molecule ->
            match molecule with
            | ResourceMolecule (CombinedResource (_, Quantum q)) when channel = q -> Some q
            | ResourceMolecule (CombinedResource (_, ClassicalData c)) when channel = c -> Some c
            | _ -> acc)
          None
          molecules
      in
      (match decoded_message with
      | Some msg -> msg
      | None -> failwith "No valid message found for decoding")
  | _ -> failwith "Decoding requires a molecule membrane" *)

let enforce_no_cloning (membrane : membrane) =
  match membrane with
  | MoleculeMembrane molecules ->
      let resources =
        List.filter_map
          (function
            | ResourceMolecule (SimpleResource r) -> Some r
            | ResourceMolecule (CombinedResource (r, _)) -> Some r
            | _ -> None)
          molecules
      in
      let unique_resources = List.sort_uniq compare resources in
      if List.length resources <> List.length unique_resources then
        failwith "Cloning violation detected in membrane"
      else
        membrane
  | _ -> membrane

let entanglement_swap (membrane : membrane) (channel_a : string) (channel_b : string) =
  match membrane with
  | MoleculeMembrane molecules ->
      print_endline ("Swapping entanglement between " ^ channel_a ^ " and " ^ channel_b);
      let molecules =
        List.map
          (function
            | ResourceMolecule (CombinedResource (SimpleResource _, Quantum q1)) when channel_a = q1 ->
                print_endline ("Swapped " ^ channel_a ^ " to " ^ channel_b);
                ResourceMolecule (CombinedResource (SimpleResource NullResource, Quantum channel_b))
            | other -> other)
          molecules
      in
      MoleculeMembrane molecules
  | _ -> failwith "Entanglement swap requires a molecule membrane"

let superdense_encode (membrane : membrane) (channel : string) (data : string) =
  encode_message channel (Quantum ("superdense:" ^ data)) membrane

let superdense_decode (membrane : membrane) (channel : string) =
  match decode_message channel membrane with
  | message when String.starts_with ~prefix:"superdense:" message ->
    String.sub message 11 (String.length message - 11)
  | _ -> failwith "Invalid superdense data" 

let quantum_teleportation (membrane : membrane) (source : string) (destination : string) =
  try
    let quantum_state = decode_message source membrane in
    encode_message destination (Quantum quantum_state) membrane
  with Failure _ ->
    failwith ("Quantum teleportation failed: No valid message found for decoding on channel: " ^ source)


let interpret (proc : process) (membrane : membrane) =
  let max_recursions = 10 in  
  let max_depth = 10 in       

  let rec interpret_with_limit proc membrane recursion_limit depth_limit =
    if recursion_limit <= 0 then (
      print_endline "Reached replication limit. Stopping replication.";
      membrane  (* Return the current membrane *)
    ) else if depth_limit <= 0 then (
      print_endline "Reached choice depth limit. Stopping branching.";
      membrane  (* Return the current membrane *)
    ) else
      match proc with
      | NullProcess ->
          print_endline "Null process. Execution complete.";
          membrane
      | ActionProcess (action, next_proc) ->
          (match action with 
          | NewChannel name ->
              print_endline ("New channel created: " ^ name)
          | Send name ->
              print_endline ("Message sent on channel: " ^ name)
          | Receive name ->
              print_endline ("Message received on channel: " ^ name)
          | Encode name ->
              print_endline ("Encoding message on channel: " ^ name);
              ignore (encode_message name (Quantum name) membrane)
          | Decode name ->
              print_endline ("Decoding message on channel: " ^ name);
              ignore (decode_message name membrane));
          interpret_with_limit next_proc membrane recursion_limit depth_limit
      | Choice (p1, p2) ->
          print_endline "Executing first branch of Choice.";
          let membrane_after_p1 =
            interpret_with_limit p1 membrane recursion_limit (depth_limit - 1)
          in
          print_endline "Executing second branch of Choice.";
          interpret_with_limit p2 membrane_after_p1 recursion_limit (depth_limit - 1)
      | Replication p ->
          print_endline "Executing replication.";
          let membrane_after_p =
            interpret_with_limit p membrane (recursion_limit - 1) depth_limit
          in
          interpret_with_limit (Replication p) membrane_after_p (recursion_limit - 1) depth_limit
  in

  ignore (interpret_with_limit proc membrane max_recursions max_depth)
  

  (* let rec interpret (proc : process) (membrane : membrane) =
  match proc with
  | NullProcess ->
      print_endline "Null process. Execution complete."
  | ActionProcess (action, next_proc) ->
    (match action with 
      | NewChannel name -> print_endline ("New channel created: " ^ name)
      | Send name -> print_endline ("Message sent on channel: " ^ name)
      | Receive name -> print_endline ("Message received on channel: " ^ name)
      | Encode name ->
          print_endline ("Encoding message on channel: " ^ name);
          ignore (encode_message name (Quantum name) membrane)
      | Decode name ->
           print_endline ("Decoding message on channel: " ^ name);
          ignore (decode_message name membrane)); 
      interpret next_proc membrane
  | Choice (p1, p2) ->
      print_endline "Executing first branch of Choice.";
      interpret p1 membrane;
      print_endline "Executing second branch of Choice.";
      interpret p2 membrane
  | Replication p ->
      print_endline "Executing replication.";
      interpret p membrane;
      interpret (Replication p) membrane *)

let example_membrane =
  MoleculeMembrane [
    ResourceMolecule (CombinedResource (SimpleResource (Quantum "chan1"), Quantum "quantum_data"))
  ]

let example_process =
  ActionProcess (
    NewChannel "chan1",
    ActionProcess (
      Encode "chan1",
      ActionProcess (
        Decode "chan1",
        NullProcess
      )
    )
  )
(* let () =
  let result = decode_message "chan1" example_membrane in
  print_endline ("Decoded message: " ^ result)
  interpret example_process example_membrane *)

let () =
  print_endline "\nRunning direct test for the interpreter...";
  interpret example_process example_membrane
