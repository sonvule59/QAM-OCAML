open Ast

type parse_error = Lex_err of string * string | Parse_err of string
(* second component: "line L, column C" of the failure *)

let position_string (lb : Lexing.lexbuf) : string =
  let p = lb.Lexing.lex_curr_p in
  Printf.sprintf "line %d, column %d" p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1)

(* ---- named-definition elaboration ----------------------------------------
   `let A = process ;` definitions are substituted away right after parsing:
   ProcessRef nodes (and bare molecule names that match a definition) are
   replaced by their bodies. A depth guard keeps self-recursive definitions
   from looping; any ProcessRef surviving elaboration is flagged by the
   typechecker as undefined. *)

let rec subst_refs (defs : (string * process) list) (depth : int) (p : process) :
    process =
  if depth <= 0 then p
  else
    match p with
    | ProcessRef n -> (
        match List.assoc_opt n defs with
        | Some body -> subst_refs defs (depth - 1) body
        | None -> p)
    | ActionProcess (a, k) -> ActionProcess (a, subst_refs defs depth k)
    | Choice (a, b) -> Choice (subst_refs defs depth a, subst_refs defs depth b)
    | Replication r -> Replication (subst_refs defs depth r)
    | NullProcess -> NullProcess

let elaborate (defs : (string * process) list) (membranes : membrane list) :
    membrane list =
  let elab_molecule = function
    | ResourceMolecule (SimpleResource n) when List.mem_assoc n defs ->
        (* a bare name in molecule position parses as a resource; promote it to
           the defined process (documented resolution rule) *)
        ProcessMolecule (subst_refs defs 32 (ProcessRef n))
    | ProcessMolecule p -> ProcessMolecule (subst_refs defs 32 p)
    | m -> m
  in
  let rec elab_membrane = function
    | MoleculeMembrane ms -> MoleculeMembrane (List.map elab_molecule ms)
    | AirlockedMembrane (l, r, rt) ->
        AirlockedMembrane (elab_membrane l, r, elab_membrane rt)
    | NullMembrane -> NullMembrane
  in
  List.map elab_membrane membranes

let parse_membranes_from_lexbuf (lb : Lexing.lexbuf) :
    (membrane list, parse_error) result =
  try
    let defs, membranes = Parser.main Lexer.token lb in
    Ok (elaborate defs membranes)
  with
  | Lexer.SyntaxError msg -> Error (Lex_err (msg, position_string lb))
  | Parser.Error -> Error (Parse_err (position_string lb))
  | Ast.Parse_error -> Error (Parse_err (position_string lb))

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

(* [action_channel] now lives in Ast (actions are structured records). *)

let reduce_encode (m : membrane) : membrane option =
  match m with
  | MoleculeMembrane molecules -> (
      match
        take_first
          (function
            | ProcessMolecule (ActionProcess ({ act = Encode _; _ }, _)) -> true
            | _ -> false)
          [] molecules
      with
      | None -> None
      | Some
          ( ProcessMolecule (ActionProcess ({ act = Encode { chan; _ }; _ }, cont)),
            rest_after_action ) ->
          let channel = chan in
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
          | ProcessMolecule (ActionProcess ({ act = Decode _; _ }, _)) -> true
          | _ -> false)
        [] molecules
    with
    | None -> None
    | Some
        ( ProcessMolecule (ActionProcess ({ act = Decode { chan; _ }; _ }, left_cont)),
          rest_left ) ->
        let channel = chan in
        let is_receiver = function
          | ProcessMolecule (ActionProcess ({ act = Receive { chan; _ }; _ }, _)) ->
              chan = channel
          | _ -> false
        in
        (match take_first is_receiver [] rest_left with
        | Some (ProcessMolecule (ActionProcess ({ act = Receive _; _ }, right_cont)), rest_right)
          ->
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
            | ProcessMolecule (ActionProcess ({ act = Decode { chan; _ }; _ }, _)) ->
                chan = boundary
            | _ -> false)
          [] left_mols
      with
      | Some (ProcessMolecule (ActionProcess ({ act = Decode _; _ }, left_cont)), left_rest)
        -> (
          match
            take_first
              (function
                | ProcessMolecule (ActionProcess ({ act = Receive { chan; _ }; _ }, _)) ->
                    chan = boundary
                | _ -> false)
              [] right_mols
          with
          | Some
              ( ProcessMolecule (ActionProcess ({ act = Receive _; _ }, right_cont)),
                right_rest ) ->
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
            | ProcessMolecule (ActionProcess ({ act = NewChannel _; _ }, _)) -> true
            | _ -> false)
          [] molecules
      with
      | Some
          ( ProcessMolecule (ActionProcess ({ act = NewChannel channel; _ }, new_cont)),
            rest_after_new ) ->
          let uses_channel = function
            | ProcessMolecule (ActionProcess (a, _)) -> action_channel a = channel
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

(* Rename each nu-bound channel in [p] to a name fresh w.r.t. [used]. Used at
   replication unfolding so independent copies of `nu c.` get distinct channels
   instead of colliding by name. *)
let freshen_nus (used : string list) (p : process) : process =
  let rec go used p =
    match p with
    | ActionProcess (({ act = NewChannel c; loc } as _a), k) ->
        let c', k' =
          if List.mem c used then
            let c' = freshen c used in
            (c', rename_process c c' k)
          else (c, k)
        in
        ActionProcess ({ act = NewChannel c'; loc }, go (c' :: used) k')
    | ActionProcess (a, k) -> ActionProcess (a, go used k)
    | Choice (a, b) -> Choice (go used a, go used b)
    | Replication r -> Replication (go used r)
    | (NullProcess | ProcessRef _) as x -> x
  in
  go used p

(* Replication: unfold `repl P` into `P | repl P`. This always grows the soup,
   so it is bounded by the fuel in `normalize` rather than reaching a fixpoint.
   The active copy's nu-binders are freshened against every name already in the
   membrane, so unfolded copies cannot spuriously interact by name capture. *)
let reduce_replication (m : membrane) : membrane option =
  match m with
  | MoleculeMembrane molecules -> (
      match
        take_first
          (function ProcessMolecule (Replication _) -> true | _ -> false)
          [] molecules
      with
      | Some (ProcessMolecule (Replication p), rest) ->
          let used =
            List.concat_map
              (function ProcessMolecule q -> names_of_process q | _ -> [])
              molecules
          in
          Some
            (MoleculeMembrane
               (ProcessMolecule (freshen_nus used p)
               :: ProcessMolecule (Replication p) :: rest))
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

let rec string_of_msg = function
  | Blank -> "o"
  | Named n -> n
  | Channeled (c, m) -> c ^ "." ^ string_of_msg m
  | Projected (c, m) -> "^" ^ c ^ "." ^ string_of_msg m
  | Residue m -> "~" ^ string_of_msg m
  | Meet (a, b) -> "(" ^ string_of_msg a ^ " & " ^ string_of_msg b ^ ")"

let string_of_action { act; _ } =
  match act with
  | NewChannel c -> "nu " ^ c ^ "."
  | Send { chan; arg } -> chan ^ "!" ^ string_of_msg arg ^ "."
  | Receive { chan; arg } -> chan ^ "?" ^ arg ^ "."
  | Encode { chan; msg } -> chan ^ "<-" ^ string_of_msg msg ^ "."
  | Decode { chan; binder } -> chan ^ "->" ^ binder ^ "."

let rec string_of_process = function
  | NullProcess -> "0"
  | ProcessRef n -> n
  | ActionProcess (a, NullProcess) -> string_of_action a
  | ActionProcess (a, p) -> string_of_action a ^ string_of_process p
  | Choice (p, q) -> string_of_process p ^ " + " ^ string_of_process q
  | Replication p -> "repl " ^ string_of_process p

let rec string_of_resource = function
  | SimpleResource s -> s
  | NullResource -> "o"
  | ChannelParty (c, m) -> c ^ "." ^ string_of_msg m
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
  | Lex_err (msg, pos) -> Printf.printf "Lexer error at %s: %s\n%!" pos msg
  | Parse_err pos -> Printf.printf "Parse error at %s.\n%!" pos

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
