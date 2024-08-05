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

(* Helper function to split a string by the first occurrence of a substring *)
let split_first_occurrence s sub =
  try
    let idx = String.index s (String.get sub 0) in
    let prefix = String.sub s 0 idx in
    let suffix = String.sub s (idx + 1) (String.length s - idx - 1) in
    (prefix, suffix)
  with Not_found -> (s, "")

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
    let (action_str, remaining_str) = split_first_occurrence s " " in
    if remaining_str = "" then ActionProcess (parse_action action_str, NullProcess)
    else ActionProcess (parse_action action_str, parse_process remaining_str)

let parse_molecule (s : string) : molecule = 
  if String.length s = 0 then NullMolecule
  else if contains_substring s "R" then ProcessMolecule (parse_process s)
  else ResourceMolecule (parse_resource s)

let parse_membrane (s : string) : membrane = 
  if String.length s = 0 then NullMembrane
  else 
    let molecules = String.split_on_char ',' s in
    MoleculeMembrane (List.map parse_molecule molecules)

(* Define the meet operation for resources *)
let meet_operation (r1 : resource) (r2 : resource) : resource =
  MeetOperation (r1, r2)

(* Implement encoding rules as a single function *)
let rec apply_encoding_rules (m : membrane) : membrane =
  match m with
  | NullMembrane -> NullMembrane
  | MoleculeMembrane molecules ->
    let rec process_molecules molecules acc =
      match molecules with
      | [] -> MoleculeMembrane acc
      | (ProcessMolecule (ActionProcess (LeftCombine alpha, process)) as mol)::rest ->
        let rec find_resource molecules =
          match molecules with
          | [] -> None
          | (ResourceMolecule (SimpleResource res) as r_mol)::rest when res = alpha -> Some r_mol
          | _::rest -> find_resource rest
        in
        (match find_resource rest with
        | Some (ResourceMolecule res) ->
          let new_molecule = ProcessMolecule (ActionProcess (LeftCombine alpha, process)) in
          process_molecules (new_molecule::rest) (acc @ [ResourceMolecule (meet_operation res (SimpleResource alpha))])
        | _ -> process_molecules rest (acc @ [mol]))
      | (ProcessMolecule (ActionProcess (RightCombine x, process)) as mol)::rest ->
        let rec find_classical molecules =
          match molecules with
          | [] -> None
          | (ResourceMolecule (SimpleResource res) as r_mol)::rest when res = x -> Some r_mol
          | _::rest -> find_classical rest
        in
        (match find_classical rest with
        | Some (ResourceMolecule res) ->
          let new_molecule = ProcessMolecule (ActionProcess (RightCombine x, process)) in
          process_molecules (new_molecule::rest) (acc @ [ResourceMolecule res])
        | _ -> process_molecules rest (acc @ [mol]))
      | (ProcessMolecule (ActionProcess (Receive x, process)) as mol)::rest ->
        let rec find_classical molecules =
          match molecules with
          | [] -> None
          | (ResourceMolecule (SimpleResource res) as r_mol)::rest when res = x -> Some r_mol
          | _::rest -> find_classical rest
        in
        (match find_classical rest with
        | Some (ResourceMolecule res) ->
          let new_molecule = ProcessMolecule (ActionProcess (Receive x, process)) in
          process_molecules (new_molecule::rest) (acc @ [ResourceMolecule res])
        | _ -> process_molecules rest (acc @ [mol]))
      | mol::rest -> process_molecules rest (acc @ [mol])
    in
    process_molecules molecules []
  | AirlockedMembrane (m1, res, m2) -> AirlockedMembrane (apply_encoding_rules m1, res, apply_encoding_rules m2)

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
let rec prompt_user () =
  print_endline "Enter process description: ";
  let input = read_line () in
  let membrane = parse_membrane input in
  let updated_membrane = apply_encoding_rules membrane in
  execute_membrane updated_membrane;
  prompt_user ()

let () = prompt_user ()
