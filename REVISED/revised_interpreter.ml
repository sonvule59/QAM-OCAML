type name = string

type message = 
  | Quantum of name
  | ClassicalData of name

type resource =
  | SimpleResource of string   (* alpha.mu *)
  | NullResource               (* o *)
  | CombinedResource of resource * message (* alpha.(mu1 o mu2) *)
  | MeetOperation of resource * resource  (* mu1 o mu2 *)

type action =
  | NewChannel of string                (* nu c. *)
  | Send of string                      (* a!i. *)
  | Receive of string                   (* delta?(x). *)
  | LeftCombine of string               (* alpha <- k. encoding *)
  | RightCombine of string              (* c -> (x). decoding *)

type process =
  | NullProcess                         (* 0 *)
  | ActionProcess of action * process   (* AR *)
  | Choice of process * process         (* R + T *)
  | Replication of process              (* !R *)

type molecule =
  | NullMolecule
  | ProcessMolecule of process
  | ResourceMolecule of resource

type membrane =
  | NullMembrane
  | MoleculeMembrane of molecule list         (* {|M|} *)
  | AirlockedMembrane of membrane * resource * membrane  (* P |[phi]| Q *)

(* Helper function to check if a string contains a substring *)
let contains_substring str sub =
  let sub_len = String.length sub in
  let rec aux i =
    if i + sub_len > String.length str then false
    else if String.sub str i sub_len = sub then true
    else aux (i + 1)
  in
  aux 0

(* Define parser functions *)
let parse_message (s : string) : message = 
  if contains_substring s "q" then Quantum s else ClassicalData s

let parse_resource (s : string) : resource = 
  match s with
  | "o" -> NullResource
  | _ -> SimpleResource s (* Simplified, need more parsing logic for CombinedResource and MeetOperation *)

let parse_action (s : string) : action = 
  if contains_substring s "nu" then NewChannel s
  else if contains_substring s "!" then Send s
  else if contains_substring s "?" then Receive s
  else if contains_substring s "<-" then LeftCombine s
  else if contains_substring s "->" then RightCombine s
  else failwith ("Unknown action: " ^ s)

let rec parse_process (s : string) : process =
  if String.length s = 0 then NullProcess
  else if contains_substring s "+" then 
    let parts = String.split_on_char '+' s in
    if List.length parts < 2 then failwith "Error parsing Choice: not enough parts"
    else Choice (parse_process (List.hd parts), parse_process (List.nth parts 1))
  else if String.get s 0 = '!' then Replication (parse_process (String.sub s 1 (String.length s - 1)))
  else
    let action_patterns = ["nu"; "!"; "?"; "<-"; "->"] in
    let rec find_action s patterns =
      match patterns with
      | [] -> failwith ("Unknown action: " ^ s)
      | p::ps -> 
        if contains_substring s p then 
          let idx = String.index s (String.get p 0) in
          let prefix = String.sub s 0 idx in
          let suffix = String.sub s (idx + String.length p) (String.length s - idx - String.length p) in
          (p, prefix, suffix)
        else find_action s ps
    in
    let (pattern, action_str, remaining_str) = find_action s action_patterns in
    let action = parse_action (pattern ^ action_str) in
    if String.trim remaining_str = "" then ActionProcess (action, NullProcess)
    else ActionProcess (action, parse_process (String.trim remaining_str))

let parse_molecule (s : string) : molecule = 
  if String.length s = 0 then NullMolecule
  else if contains_substring s "!" || contains_substring s "?" || contains_substring s "<-" || contains_substring s "->" || contains_substring s "nu" then
    ProcessMolecule (parse_process s)
  else ResourceMolecule (parse_resource s)

let parse_membrane (s : string) : membrane = 
  if String.length s = 0 then NullMembrane
  else 
    let molecules = String.split_on_char ',' s in
    MoleculeMembrane (List.map parse_molecule molecules)

(* Define the meet operation for resources *)
let meet_operation (r1 : resource) (r2 : resource) : resource =
  MeetOperation (r1, r2)

(* Logging function to print the membrane state *)
let print_membrane_state (m : membrane) =
  match m with
  | NullMembrane -> print_endline "NullMembrane"
  | MoleculeMembrane molecules ->
    List.iter (fun m -> match m with
      | NullMolecule -> print_endline "NullMolecule"
      | ProcessMolecule _ -> print_endline "ProcessMolecule"
      | ResourceMolecule _ -> print_endline "ResourceMolecule"
    ) molecules
  | AirlockedMembrane (_, _, _) -> print_endline "AirlockedMembrane"

(* Implement encoding rules as a single function *)
let rec apply_encoding_rules (m : membrane) : membrane =
  match m with
  | NullMembrane -> NullMembrane
  (* If the membrane is NullMembrane, return it as is. *)

  | MoleculeMembrane molecules ->
    let rec process_molecules molecules acc =
      match molecules with
      | [] -> MoleculeMembrane acc
      (* If there are no more molecules to process, return a MoleculeMembrane with the accumulated molecules. *)

      | (ProcessMolecule (ActionProcess (LeftCombine alpha, process)) as mol)::rest ->
        let rec find_resource molecules =
          match molecules with
          | [] -> None
          (* If no resource molecule is found, return None. *)

          | (ResourceMolecule (SimpleResource res) as r_mol)::rest when res = alpha -> Some r_mol
          (* If a ResourceMolecule with the same resource name (alpha) is found, return it. *)

          | _::rest -> find_resource rest
          (* Continue searching in the rest of the molecules. *)
        in
        (match find_resource rest with
        | Some (ResourceMolecule res) ->
          let new_molecule = ProcessMolecule (ActionProcess (LeftCombine alpha, process)) in
          print_endline "Applying QLocal:";
          process_molecules (new_molecule::rest) (acc @ [ResourceMolecule (meet_operation res (SimpleResource alpha))])
          (* If a matching resource is found, create a new ProcessMolecule with the LeftCombine action and update the resource.
             Continue processing with the rest of the molecules and the new accumulated list. *)

        | _ -> process_molecules rest (acc @ [mol]))
        (* If no matching resource is found, continue processing the rest of the molecules with the current one added to the accumulated list. *)

      | (ProcessMolecule (ActionProcess (RightCombine x, process)) as mol)::rest ->
        let rec find_classical molecules =
          match molecules with
          | [] -> None
          (* If no resource molecule is found, return None. *)

          | (ResourceMolecule (SimpleResource res) as r_mol)::rest when res = x -> Some r_mol
          (* If a ResourceMolecule with the same resource name (x) is found, return it. *)

          | _::rest -> find_classical rest
          (* Continue searching in the rest of the molecules. *)
        in
        (match find_classical rest with
        | Some (ResourceMolecule res) ->
          let new_molecule = ProcessMolecule (ActionProcess (RightCombine x, process)) in
          print_endline "Applying Encode:";
          process_molecules (new_molecule::rest) (acc @ [ResourceMolecule res])
          (* If a matching resource is found, create a new ProcessMolecule with the RightCombine action and update the resource.
             Continue processing with the rest of the molecules and the new accumulated list. *)

        | _ -> process_molecules rest (acc @ [mol]))
        (* If no matching resource is found, continue processing the rest of the molecules with the current one added to the accumulated list. *)

      | (ProcessMolecule (ActionProcess (Receive x, process)) as mol)::rest ->
        let rec find_classical molecules =
          match molecules with
          | [] -> None
          (* If no resource molecule is found, return None. *)

          | (ResourceMolecule (SimpleResource res) as r_mol)::rest when res = x -> Some r_mol
          (* If a ResourceMolecule with the same resource name (x) is found, return it. *)

          | _::rest -> find_classical rest
          (* Continue searching in the rest of the molecules. *)
        in
        (match find_classical rest with
        | Some (ResourceMolecule res) ->
          let new_molecule = ProcessMolecule (ActionProcess (Receive x, process)) in
          process_molecules (new_molecule::rest) (acc @ [ResourceMolecule res])
          (* If a matching resource is found, create a new ProcessMolecule with the Receive action and update the resource.
             Continue processing with the rest of the molecules and the new accumulated list. *)

        | _ -> process_molecules rest (acc @ [mol]))
        (* If no matching resource is found, continue processing the rest of the molecules with the current one added to the accumulated list. *)

      | mol::rest -> process_molecules rest (acc @ [mol])
      (* For any other molecule, continue processing with the rest of the molecules with the current one added to the accumulated list. *)
    in
    let result = process_molecules molecules [] in
    print_endline "Membrane state after rule application:";
    print_membrane_state result;
    result
    (* Start processing the molecules with an empty accumulated list. *)

  | AirlockedMembrane (m1, res, m2) -> 
    print_endline "Processing AirlockedMembrane:";
    let m1' = apply_encoding_rules m1 in
    let m2' = apply_encoding_rules m2 in
    AirlockedMembrane (m1', res, m2')
  (* If the membrane is an AirlockedMembrane, recursively apply the encoding rules to both inner membranes. *)

(* Implement Cohere rule *)
let apply_cohere_rule (m : membrane) : membrane =
  match m with
  | MoleculeMembrane molecules ->
    (* Find and process the two sub-membranes that match the Cohere rule *)
    let rec process_cohere molecules acc =
      match molecules with
      | [] -> MoleculeMembrane acc
      | (ProcessMolecule (ActionProcess (NewChannel c1, p1)) as mol1)::rest1 ->
        (match rest1 with
        | (ProcessMolecule (ActionProcess (NewChannel c2, p2)) as mol2)::rest2 when c1 = c2 ->
          print_endline "Applying Cohere:";
          process_cohere rest2 (acc @ [ProcessMolecule p1; ResourceMolecule (SimpleResource c1); ProcessMolecule p2])
        | _ -> process_cohere rest1 (acc @ [mol1]))
      | mol::rest -> process_cohere rest (acc @ [mol])
    in
    let result = process_cohere molecules [] in
    print_endline "Membrane state after Cohere application:";
    print_membrane_state result;
    result
  | _ -> m

(* Define state transitions *)
let rec execute_process (p : process) : unit = 
  match p with
  | NullProcess -> ()
  | ActionProcess (a, p) -> 
    execute_action a;
    execute_process p
  | Choice (p1, p2) -> 
    execute_process p1; (* Simplified to always choose the first process *)
    execute_process p2
  | Replication p -> 
    while true do
      execute_process p
    done

and execute_action (a : action) : unit = 
  match a with
  | NewChannel s -> print_endline ("Creating new channel: " ^ s)
  | Send s -> print_endline ("Sending message: " ^ s)
  | Receive s -> print_endline ("Receiving message: " ^ s)
  | LeftCombine s -> print_endline ("Encoding message: " ^ s)
  | RightCombine s -> print_endline ("Decoding message: " ^ s)

let rec execute_membrane (m : membrane) : unit =
  match m with
  | NullMembrane -> ()
  | MoleculeMembrane molecules -> 
    List.iter (fun mol -> match mol with
      | NullMolecule -> ()
      | ProcessMolecule p -> execute_process p
      | ResourceMolecule r -> ()) molecules
  | AirlockedMembrane (m1, r, m2) -> 
    execute_membrane m1;
    execute_membrane m2

(* Equivalence checker *)
let prompt_user () =
  print_endline "Enter process description: ";
  let input = read_line () in
  let membrane = parse_membrane input in
  print_endline "Parsed membrane:";
  print_membrane_state membrane;
  let updated_membrane = apply_encoding_rules membrane in
  print_endline "Final membrane after rule applications:";
  print_membrane_state updated_membrane;
  let final_membrane = apply_cohere_rule updated_membrane in
  print_endline "Final membrane after Cohere application:";
  print_membrane_state final_membrane;
  execute_membrane final_membrane;
  print_endline "Execution complete."

let () = prompt_user ()
