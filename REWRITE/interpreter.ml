open Ast 
open Lexer
open Parser

type msg = string

let parse_with_error lexbuf =
  try 
    Parser.main Lexer.token lexbuf 
  with
  | Lexer.SyntaxError msg ->
    Printf.fprintf stderr "%s%!" msg;
    exit (-1)
  | Parser.ParseError ->
    Printf.fprintf stderr "Parse error at %s\n%!" (Lexing.lexeme lexbuf);
    exit (-1)

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

(* Interpret function *)
let rec interpret (rule: string) (m : membrane) : membrane =
  match rule with
  | "ENCODE" ->
    (match m with
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
            print_endline "Applying ENCODE:";
            process_molecules rest (acc @ [ProcessMolecule process; ResourceMolecule (meet_operation res (SimpleResource alpha))])
          | _ -> process_molecules rest (acc @ [mol]))
        | mol::rest -> process_molecules rest (acc @ [mol])
      in
      let result = process_molecules molecules [] in
      print_endline "Membrane state after ENCODE rule application:";
      print_membrane_state result;
      result
    | _ -> m)
  | "DECODE" ->
    (match m with
    | AirlockedMembrane (MoleculeMembrane left_mols, SimpleResource res, MoleculeMembrane right_mols) ->
      let rec find_process molecule_list =
        match molecule_list with
        | [] -> None
        | (ProcessMolecule (ActionProcess (RightCombine x, process)) as mol)::rest when x = res -> Some (mol, process, rest)
        | _::rest -> find_process rest
      in
      let rec find_receive molecule_list =
        match molecule_list with
        | [] -> None
        | (ProcessMolecule (ActionProcess (Receive x, process)) as mol)::rest when x = res -> Some (mol, process, rest)
        | _::rest -> find_receive rest
      in
      (match find_process left_mols, find_receive right_mols with
      | Some (left_mol, left_process, left_rest), Some (right_mol, right_process, right_rest) ->
        print_endline "Applying DECODE:";
        let new_left = left_process in
        let new_right = ActionProcess (Receive res, right_process) in
        AirlockedMembrane (MoleculeMembrane (ProcessMolecule new_left :: left_rest), SimpleResource res, MoleculeMembrane (ProcessMolecule new_right :: right_rest))
      | _ -> m)
    | _ -> m)
  | "COHERE" ->
    (match m with
    | MoleculeMembrane molecules ->
      let rec find_channels mols acc =
        match mols with
        | [] -> acc
        | (ProcessMolecule (ActionProcess (NewChannel c, process)) as mol)::rest ->
          find_channels rest ((mol, mol, mol) :: acc)
        | _::rest -> find_channels rest acc
      in
      let channels = find_channels molecules [] in
      let rec cohere chs =
        match chs with
        | [] -> m
        | (left_mol, left_process, right_process)::rest ->
          let right_mols = List.filter (fun (_, _, right_process) -> match right_process with ProcessMolecule (ActionProcess (NewChannel c, _)) -> true | _ -> false) chs in
          match right_mols with
          | [] -> cohere rest
          | (right_mol, _, _)::right_rest ->
            print_endline "Applying COHERE:";
            MoleculeMembrane (left_process :: right_process :: (List.filter (fun (m, _, _) -> m <> left_mol && m <> right_mol) chs |> List.map (fun (m, _, _) -> m)))
          | _ -> cohere rest
      in
      cohere channels
    | _ -> m)
  | _ -> failwith "Unknown rule"

let check_equivalence_rules (m : Ast.membrane)(p : Ast.process) : Ast.equivalence_result =
  let id1_rule (m: membrane) =
    match m with
    | MoleculeMembrane [] -> Equivalent
    | _ -> NotEquivalent "ID1 rule failed"
    (* Maybe add an eval statement here???? *)
  in
  let rec id2_rule (p: process) =
    match p with
    | NullProcess -> Equivalent
    | _ -> NotEquivalent "ID2 rule failed"
    (* Maybe add an eval statement here???? *)
  in
  let split_rule (m: membrane) =
    match m with
    | MoleculeMembrane [MoleculeMembrane m1; MoleculeMembrane m2] -> Equivalent
    | _ -> NotEquivalent "Split rule failed"
  in
  let cl_rule (p: process) =
    match p with
    | Choice (p1, p2) -> Equivalent
    | _ -> NotEquivalent "CL rule failed"
  in
  let cr_rule (p: process) =
    match p with
    | Choice (p1, p2) -> Equivalent
    | _ -> NotEquivalent "CR rule failed"
  in
  let mt_rule (p: process) =
    match p with
    | Replication p -> Equivalent
    | _ -> NotEquivalent "MT rule failed"
  in
  let nt_rule (p: process) =
    match p with
    | Replication p -> Equivalent
    | _ -> NotEquivalent "NT rule failed"
  in
  let decohere_rule (m: membrane) =
    match m with
    | NullMembrane -> Equivalent
    | _ -> NotEquivalent "Decohere rule failed"
  in
  let com_rule (m: membrane) =
    match m with
    | MoleculeMembrane [ProcessMolecule (ActionProcess (Send _, _)); ProcessMolecule (ActionProcess (Receive _, _))] -> Equivalent
    | _ -> NotEquivalent "Com rule failed"
  in

  let rec results = [
    id1_rule m;
    id2_rule p;
    split_rule m;
    cl_rule p;
    cr_rule p;
    mt_rule p;
    nt_rule p;
    decohere_rule m;
    com_rule m;
  ] in

  let failed_results = List.filter (fun r -> match r with | Equivalent -> false | NotEquivalent _ -> true) results in
  if List.length failed_results == 0 then
    Equivalent
  else
    NotEquivalent (String.concat "; " (List.map (function NotEquivalent msg -> msg | _ -> "") failed_results))

(* Main equivalence checker *)
let equivalence_step (input: string) =
  let lexbuf = Lexing.from_string input in
  let membrane = parse_with_error lexbuf in
  match check_equivalence_rules membrane NullProcess with
  | Equivalent -> print_endline "Input is equivalent according to all rules."
  | NotEquivalent msg -> print_endline ("Not Equivalent: " ^ msg)

  
(* Interactive interpreter *)
let rec interactive_prompt (membrane : membrane) =
  print_endline "\nOptions:";
  print_endline "1. Enter a new process description";
  print_endline "2. Apply ENCODE rule";
  print_endline "3. Apply DECODE rule";
  print_endline "4. Apply COHERE rule";
  print_endline "5. Check equivalence";
  print_endline "6. View current membrane state";
  print_endline "7. Execute membrane";
  print_endline "8. Exit";
  print_string "Choose an option: ";
  match read_line () with
  | "1" ->
    print_string "Enter process description: ";
    let input = read_line () in
    let lexbuf = from_string input in
    let new_membrane = parse_with_error lexbuf in
    interactive_prompt (MoleculeMembrane ((match membrane with
      | MoleculeMembrane m -> m
      | _ -> []) @ (match new_membrane with
      | MoleculeMembrane m -> m
      | _ -> [])))
  | "2" ->
    let updated_membrane = interpret "ENCODE" membrane in
    print_endline "Updated membrane after applying ENCODE rule:";
    print_membrane_state updated_membrane;
    interactive_prompt updated_membrane
  | "3" ->
    let updated_membrane = interpret "DECODE" membrane in
    print_endline "Updated membrane after applying DECODE rule:";
    print_membrane_state updated_membrane;
    interactive_prompt updated_membrane
  | "4" ->
    let updated_membrane = interpret "COHERE" membrane in
    print_endline "Updated membrane after applying COHERE rule:";
    print_membrane_state updated_membrane;
    interactive_prompt updated_membrane
  | "5" ->
    print_string "Enter equivalence description: ";
    let input = read_line () in
    equivalence_step input;
    interactive_prompt membrane
  | "6" ->
    print_endline "Current membrane state:";
    print_membrane_state membrane;
    interactive_prompt membrane
  | "7" ->
    print_endline "Executing membrane...";
    (* Here you should define the function execute_membrane if it exists in your logic. *)
    (* execute_membrane membrane; *)
    print_endline "Execution complete.";
    interactive_prompt membrane
  | "8" -> print_endline "Exiting."
  | _ ->
    print_endline "Invalid option. Please choose a valid option.";
    interactive_prompt membrane

let () =
  print_endline "Welcome to the interactive interpreter!";
  let initial_membrane = NullMembrane in 
  interactive_prompt initial_membrane
