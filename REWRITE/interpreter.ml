open Ast

type parse_error = Lex_err of string | Parse_err

let parse_membranes_from_lexbuf (lb : Lexing.lexbuf) :
    (membrane list, parse_error) result =
  try Ok (Parser.main Lexer.token lb) with
  | Lexer.SyntaxError msg -> Error (Lex_err msg)
  | Parser.Error -> Error Parse_err
  | Ast.Parse_error -> Error Parse_err

let parse_membranes_from_string (input : string) :
    (membrane list, parse_error) result =
  parse_membranes_from_lexbuf (Lexing.from_string input)

let meet_operation (r1 : resource) (r2 : resource) : resource =
  MeetOperation (r1, r2)

let rec take_first pred acc items =
  match items with
  | [] -> None
  | x :: xs ->
      if pred x then Some (x, List.rev_append acc xs)
      else take_first pred (x :: acc) xs

let before_substring (s : string) (needle : string) : string =
  let s_len = String.length s in
  let n_len = String.length needle in
  let rec aux i =
    if i + n_len > s_len then s
    else if String.sub s i n_len = needle then String.sub s 0 i
    else aux (i + 1)
  in
  aux 0

let action_channel (action : action) : string =
  match action with
  | NewChannel c -> c
  | Send { chan; _ } -> chan
  | Receive { chan; _ } -> chan
  | LeftCombine s -> before_substring s "<-"
  | RightCombine s -> before_substring s "->"

let reduce_encode (m : membrane) : membrane option =
  match m with
  | MoleculeMembrane molecules -> (
      match
        take_first
          (function
            | ProcessMolecule (ActionProcess (LeftCombine _, _)) -> true
            | _ -> false)
          [] molecules
      with
      | None -> None
      | Some (ProcessMolecule (ActionProcess (LeftCombine a, cont)), rest_after_action) ->
          let channel = action_channel (LeftCombine a) in
          let is_target_resource = function
            | ResourceMolecule (SimpleResource rname) -> rname = channel
            | _ -> false
          in
          (match take_first is_target_resource [] rest_after_action with
          | Some (ResourceMolecule resource, rest_after_resource) ->
              let encoded_resource =
                ResourceMolecule (meet_operation resource (SimpleResource channel))
              in
              Some
                (MoleculeMembrane
                   (ProcessMolecule cont :: encoded_resource :: rest_after_resource))
          | _ -> None)
      | Some _ -> None)
  | _ -> None

let reduce_decode (m : membrane) : membrane option =
  let reduce_molecule_membrane molecules =
    match
      take_first
        (function
          | ProcessMolecule (ActionProcess (RightCombine _, _)) -> true
          | _ -> false)
        [] molecules
    with
    | None -> None
    | Some (ProcessMolecule (ActionProcess (RightCombine encoded, left_cont)), rest_left) ->
        let channel = action_channel (RightCombine encoded) in
        let is_receiver = function
          | ProcessMolecule (ActionProcess (Receive recv, _)) ->
              action_channel (Receive recv) = channel
          | _ -> false
        in
        (match take_first is_receiver [] rest_left with
        | Some (ProcessMolecule (ActionProcess (Receive _, right_cont)), rest_right) ->
            Some
              (MoleculeMembrane
                 (ProcessMolecule left_cont :: ProcessMolecule right_cont :: rest_right))
        | _ -> None)
    | Some _ -> None
  in
  match m with
  | MoleculeMembrane molecules -> reduce_molecule_membrane molecules
  | AirlockedMembrane
      (MoleculeMembrane left_mols, SimpleResource boundary, MoleculeMembrane right_mols)
    -> (
      match
        take_first
          (function
            | ProcessMolecule (ActionProcess (RightCombine encoded, _)) ->
                action_channel (RightCombine encoded) = boundary
            | _ -> false)
          [] left_mols
      with
      | Some (ProcessMolecule (ActionProcess (RightCombine _, left_cont)), left_rest) -> (
          match
            take_first
              (function
                | ProcessMolecule (ActionProcess (Receive recv, _)) ->
                    action_channel (Receive recv) = boundary
                | _ -> false)
              [] right_mols
          with
          | Some (ProcessMolecule (ActionProcess (Receive _, right_cont)), right_rest) ->
              Some
                (AirlockedMembrane
                   ( MoleculeMembrane (ProcessMolecule left_cont :: left_rest),
                     SimpleResource boundary,
                     MoleculeMembrane (ProcessMolecule right_cont :: right_rest) ))
          | _ -> None)
      | _ -> None)
  | _ -> None

let reduce_cohere (m : membrane) : membrane option =
  match m with
  | MoleculeMembrane molecules -> (
      match
        take_first
          (function
            | ProcessMolecule (ActionProcess (NewChannel _, _)) -> true
            | _ -> false)
          [] molecules
      with
      | Some (ProcessMolecule (ActionProcess (NewChannel channel, new_cont)), rest_after_new)
        ->
          let uses_channel = function
            | ProcessMolecule (ActionProcess (act, _)) -> action_channel act = channel
            | _ -> false
          in
          (match take_first uses_channel [] rest_after_new with
          | Some (ProcessMolecule (ActionProcess (_, partner_cont)), rest_after_partner) ->
              Some
                (MoleculeMembrane
                   (ProcessMolecule new_cont
                   :: ProcessMolecule partner_cont :: rest_after_partner))
          | _ -> None)
      | _ -> None)
  | _ -> None

let interpret (rule : string) (m : membrane) : membrane =
  match rule with
  | "ENCODE" -> (match reduce_encode m with Some m' -> m' | None -> m)
  | "DECODE" -> (match reduce_decode m with Some m' -> m' | None -> m)
  | "COHERE" -> (match reduce_cohere m with Some m' -> m' | None -> m)
  | _ -> failwith "Unknown rule"

(* Choice: MVP internal-choice semantics. `P + Q` commits to the left branch.
   Full left/right nondeterminism would require reduce_once to return multiple
   successors (a set/list), which the membrane-option relation cannot express;
   that is deferred. *)
let reduce_choice (m : membrane) : membrane option =
  match m with
  | MoleculeMembrane molecules -> (
      match
        take_first
          (function ProcessMolecule (Choice _) -> true | _ -> false)
          [] molecules
      with
      | Some (ProcessMolecule (Choice (left, _right)), rest) ->
          Some (MoleculeMembrane (ProcessMolecule left :: rest))
      | _ -> None)
  | _ -> None

(* Replication: unfold `repl P` into `P | repl P`. This always grows the soup,
   so it is bounded by the fuel in `normalize` rather than reaching a fixpoint. *)
let reduce_replication (m : membrane) : membrane option =
  match m with
  | MoleculeMembrane molecules -> (
      match
        take_first
          (function ProcessMolecule (Replication _) -> true | _ -> false)
          [] molecules
      with
      | Some (ProcessMolecule (Replication p), rest) ->
          Some
            (MoleculeMembrane
               (ProcessMolecule p :: ProcessMolecule (Replication p) :: rest))
      | _ -> None)
  | _ -> None

let reduce_once (m : membrane) : membrane option =
  match reduce_encode m with
  | Some m' -> Some m'
  | None -> (
      match reduce_decode m with
      | Some m' -> Some m'
      | None -> (
          match reduce_cohere m with
          | Some m' -> Some m'
          | None -> (
              match reduce_choice m with
              | Some m' -> Some m'
              | None -> reduce_replication m)))

let normalize ?(fuel = 128) (m : membrane) : membrane =
  let rec loop steps current =
    if steps = 0 then current
    else
      match reduce_once current with
      | Some next when next <> current -> loop (steps - 1) next
      | _ -> current
  in
  loop fuel m

let string_of_message = function
  | Quantum s -> "q:" ^ s
  | ClassicalData s -> "c:" ^ s

let string_of_action = function
  | NewChannel c -> "nu " ^ c ^ "."
  | Send { chan; arg } -> chan ^ "!" ^ arg ^ "."
  | Receive { chan; arg } -> chan ^ "?" ^ arg ^ "."
  | LeftCombine s -> s ^ "."
  | RightCombine s -> s ^ "."

let rec string_of_process = function
  | NullProcess -> "0"
  | ActionProcess (a, NullProcess) -> string_of_action a
  | ActionProcess (a, p) -> string_of_action a ^ string_of_process p
  | Choice (p, q) -> string_of_process p ^ " + " ^ string_of_process q
  | Replication p -> "repl " ^ string_of_process p

let rec string_of_resource = function
  | SimpleResource s -> s
  | NullResource -> "o"
  | CombinedResource (r, m) -> string_of_resource r ^ ".(" ^ string_of_message m ^ ")"
  | MeetOperation (r1, r2) -> string_of_resource r1 ^ " & " ^ string_of_resource r2

let string_of_molecule = function
  | NullMolecule -> "0"
  | ProcessMolecule p -> string_of_process p
  | ResourceMolecule r -> string_of_resource r

let rec string_of_membrane = function
  | NullMembrane -> "{}"
  | MoleculeMembrane ms ->
      "{ " ^ String.concat ", " (List.map string_of_molecule ms) ^ " }"
  | AirlockedMembrane (l, r, rt) ->
      "|[ " ^ string_of_membrane l ^ ", " ^ string_of_resource r ^ ", "
      ^ string_of_membrane rt ^ " ]|"

let print_membrane_state (m : membrane) = print_endline (string_of_membrane m)

(* Phase 2 equivalence policy: normalize both sides (same fuel), canonicalize,
   then structurally compare. See README "Equivalence policy". *)
let check_equivalence_between_membranes ?(fuel = 128) (m1 : membrane)
    (m2 : membrane) : equivalence_result =
  let n1 = normalize ~fuel m1 in
  let n2 = normalize ~fuel m2 in
  if CheckEquivalence.equivalent n1 n2 then Equivalent
  else NotEquivalent "Canonical normal forms differ."

let print_parse_error = function
  | Lex_err msg -> Printf.printf "Lexer error: %s\n%!" msg
  | Parse_err -> print_endline "Parse error."

let equivalence_step (input : string) =
  match parse_membranes_from_string input with
  | Error e -> print_parse_error e
  | Ok [] -> print_endline "No membrane found in input."
  | Ok [m] ->
      (* Single membrane: report whether it is already a normal form. We compare
         m against normalize(m) canonically WITHOUT normalizing the left side, so
         this is not the tautology "normalize m = normalize (normalize m)". *)
      let nf = normalize m in
      if CheckEquivalence.equivalent m nf then
        print_endline "Already a normal form (stable under reduction)."
      else
        print_endline ("Not a normal form; reduces to: " ^ string_of_membrane nf)
  | Ok (m1 :: m2 :: _) -> (
      match check_equivalence_between_membranes m1 m2 with
      | Equivalent ->
          print_endline
            "Equivalent: membranes have the same canonical normal form."
      | NotEquivalent msg -> print_endline ("Not equivalent: " ^ msg))

let print_system_state (membranes : membrane list) =
  if membranes = [] then print_endline "No membranes loaded."
  else
    List.iteri
      (fun i m ->
        Printf.printf "Membrane %d: " (i + 1);
        print_membrane_state m)
      membranes

let apply_rule_to_membranes (rule : string) (membranes : membrane list) :
    membrane list =
  List.map (interpret rule) membranes

let execute_membrane (m : membrane) : membrane =
  normalize m

let execute_all (membranes : membrane list) : membrane list =
  List.map execute_membrane membranes

let rec interactive_prompt (membranes : membrane list) =
  print_endline "\nOptions:";
  print_endline "1. Enter a new process/membrane description";
  print_endline "2. Apply ENCODE rule";
  print_endline "3. Apply DECODE rule";
  print_endline "4. Apply COHERE rule";
  print_endline "5. Check equivalence";
  print_endline "6. View current membrane state";
  print_endline "7. Execute membrane(s) to normal form";
  print_endline "8. Exit";
  print_string "Choose an option: ";
  match read_line () with
  | "1" ->
      print_string "Enter membrane description: ";
      let input = read_line () in
      (match parse_membranes_from_string input with
      | Error e ->
          print_parse_error e;
          interactive_prompt membranes
      | Ok parsed -> interactive_prompt (membranes @ parsed))
  | "2" ->
      let updated = apply_rule_to_membranes "ENCODE" membranes in
      print_endline "Applied ENCODE.";
      print_system_state updated;
      interactive_prompt updated
  | "3" ->
      let updated = apply_rule_to_membranes "DECODE" membranes in
      print_endline "Applied DECODE.";
      print_system_state updated;
      interactive_prompt updated
  | "4" ->
      let updated = apply_rule_to_membranes "COHERE" membranes in
      print_endline "Applied COHERE.";
      print_system_state updated;
      interactive_prompt updated
  | "5" ->
      print_string
        "Enter one membrane (stability check) or two membranes separated by a \
         comma: ";
      let input = read_line () in
      equivalence_step input;
      interactive_prompt membranes
  | "6" ->
      print_endline "Current membrane state:";
      print_system_state membranes;
      interactive_prompt membranes
  | "7" ->
      let executed = execute_all membranes in
      print_endline "Execution complete.";
      print_system_state executed;
      interactive_prompt executed
  | "8" -> print_endline "Exiting."
  | _ ->
      print_endline "Invalid option. Please choose a valid option.";
      interactive_prompt membranes
