#use "topfind";;
#require "str";;
open Str

(* Define the types *)
type resource =
  | SimpleResource of string * string   (* α.μ *)
  | CombinedResource of string * resource * resource (* α.(μ1 ⊙ μ2) *)
  | NullResource                        (* ◦ *)
  | MeetOperation of resource * resource  (* μ1 ⊙ μ2 *)

type action =
  | NewChannel of string                (* ν c. *)
  | Send of string * string             (* a!ι. *)
  | Receive of string * string          (* α?(x). *)
  | LeftCombine of string * string      (* α ◁ κ. *) (* This is encoding *)
  | RightCombine of string * string     (* c ▷ (x). *) (* This is decoding *)

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
  | AirlockedMembrane of membrane * resource * membrane  (* P |[ϕ]| Q *)

(* Helper function to apply the meet operation *)
let rec meet_operation res1 res2 = match (res1, res2) with
  | (NullResource, r) -> r
  | (r, NullResource) -> r
  | (SimpleResource (alpha1, mu1), SimpleResource (alpha2, mu2)) when alpha1 = alpha2 ->
      CombinedResource (alpha1, SimpleResource (alpha1, mu1), SimpleResource (alpha2, mu2))
  | _ -> MeetOperation (res1, res2)

(* Function to convert resource to string *)
let rec string_of_resource = function
  | SimpleResource (alpha, mu) -> alpha ^ "." ^ mu
  | CombinedResource (alpha, res1, res2) -> alpha ^ ".(" ^ (string_of_resource res1) ^ " ⊙ " ^ (string_of_resource res2) ^ ")"
  | NullResource -> "◦"
  | MeetOperation (res1, res2) -> "(" ^ (string_of_resource res1) ^ " ⊙ " ^ (string_of_resource res2) ^ ")"

(* Function to convert process to string *)
let rec string_of_process = function
  | NullProcess -> "0"
  | ActionProcess (LeftCombine (alpha, mu), p) -> alpha ^ " ◁ " ^ mu ^ ". " ^ (string_of_process p)
  | ActionProcess (RightCombine (c, x), p) -> c ^ " ▷ (" ^ x ^ "). " ^ (string_of_process p)
  | ActionProcess (Receive (c, x), p) -> c ^ "?" ^ x ^ ". " ^ (string_of_process p)
  | ActionProcess (NewChannel c, p) -> "ν " ^ c ^ ". " ^ (string_of_process p)
  | ActionProcess (Send (a, i), p) -> a ^ "!" ^ i ^ ". " ^ (string_of_process p)
  | Choice (p1, p2) -> (string_of_process p1) ^ " + " ^ (string_of_process p2)
  | Replication p -> "!" ^ (string_of_process p)

(* Function to convert molecule to string *)
let string_of_molecule = function
  | NullMolecule -> "NullMolecule"
  | ProcessMolecule p -> "ProcessMolecule(" ^ (string_of_process p) ^ ")"
  | ResourceMolecule res -> "ResourceMolecule(" ^ (string_of_resource res) ^ ")"

(* Function to convert membrane to string *)
let rec string_of_membrane = function
  | NullMembrane -> "NullMembrane"
  | MoleculeMembrane molecules -> "MoleculeMembrane(" ^ (String.concat ", " (List.map string_of_molecule molecules)) ^ ")"
  | AirlockedMembrane (p, r, q) -> "AirlockedMembrane(" ^ (string_of_membrane p) ^ ", " ^ (string_of_resource r) ^ ", " ^ (string_of_membrane q) ^ ")"

(* Function to print the current state of a resource *)
let rec print_resource = function
  | SimpleResource (alpha, mu) -> print_string (alpha ^ "." ^ mu)
  | CombinedResource (alpha, res1, res2) -> 
      print_string (alpha ^ ".("); print_resource res1; print_string " ⊙ "; print_resource res2; print_string ")"
  | NullResource -> print_string "◦"
  | MeetOperation (res1, res2) ->
      print_string "("; print_resource res1; print_string " ⊙ "; print_resource res2; print_string ")"

(* Function to print the current state of a process *)
let rec print_process = function
  | NullProcess -> print_string "0"
  | ActionProcess (LeftCombine (alpha, mu), p) -> 
      print_string (alpha ^ " ◁ " ^ mu ^ ". "); print_process p
  | ActionProcess (RightCombine (c, x), p) -> 
      print_string (c ^ " ▷ (" ^ x ^ "). "); print_process p
  | ActionProcess (Receive (c, x), p) -> 
      print_string (c ^ "?" ^ x ^ ". "); print_process p
  | ActionProcess (NewChannel c, p) ->
      print_string ("ν " ^ c ^ ". "); print_process p
  | ActionProcess (Send (a, i), p) ->
      print_string (a ^ "!" ^ i ^ ". "); print_process p
  | Choice (p1, p2) -> 
      print_process p1; print_string " + "; print_process p2
  | Replication p -> 
      print_string "!"; print_process p

(* Function to print the current state of a molecule *)
let print_molecule = function
  | NullMolecule -> print_string "NullMolecule"
  | ProcessMolecule p -> print_string "ProcessMolecule("; print_process p; print_string ")"
  | ResourceMolecule res -> print_string "ResourceMolecule("; print_resource res; print_string ")"

(* Function to print the current state of a membrane *)
let rec print_membrane = function
  | NullMembrane -> print_string "NullMembrane"
  | MoleculeMembrane molecules ->
      print_string "MoleculeMembrane("; 
      List.iter (fun m -> print_molecule m; print_string ", ") molecules; 
      print_string ")"
  | AirlockedMembrane (p, r, q) ->
      print_string "AirlockedMembrane("; 
      print_membrane p; 
      print_string ", "; 
      print_resource r; 
      print_string ", "; 
      print_membrane q; 
      print_string ")"

(* Helper function to apply a substitution in a process *)
let rec substitute var value = function
  | NullProcess -> NullProcess
  | ActionProcess (a, p) -> ActionProcess (a, substitute var value p)
  | Choice (p1, p2) -> Choice (substitute var value p1, substitute var value p2)
  | Replication p -> Replication (substitute var value p)

(* Parsing Resources *)
let rec parse_resource str =
  let null_resource_pattern = Str.regexp "◦" in
  let simple_resource_pattern = Str.regexp "\\([a-zA-Z]+\\)\\.\\([a-zA-Z0-9]+\\)" in
  let combined_resource_pattern = Str.regexp "\\([a-zA-Z]+\\)\\.\\((.*) ⊙ (.*)\\)" in
  if Str.string_match null_resource_pattern str 0 then
    NullResource
  else if Str.string_match simple_resource_pattern str 0 then
    let alpha = Str.matched_group 1 str in
    let mu = Str.matched_group 2 str in
    SimpleResource (alpha, mu)
  else if Str.string_match combined_resource_pattern str 0 then
    let alpha = Str.matched_group 1 str in
    let mu1 = parse_resource (Str.matched_group 2 str) in
    let mu2 = parse_resource (Str.matched_group 3 str) in
    CombinedResource (alpha, mu1, mu2)
  else
    failwith "Invalid resource format"

(* Parsing Actions *)
let parse_action str =
  let new_channel_pattern = Str.regexp "ν \\([a-zA-Z]+\\)\\." in
  let send_pattern = Str.regexp "\\([a-zA-Z]+\\)!\\([a-zA-Z0-9]+\\)\\." in
  let receive_pattern = Str.regexp "\\([a-zA-Z]+\\)\\?\\([a-zA-Z0-9]+\\)\\." in
  let left_combine_pattern = Str.regexp "\\([a-zA-Z]+\\) ◁ \\([a-zA-Z0-9]+\\)\\." in
  let right_combine_pattern = Str.regexp "\\([a-zA-Z]+\\) ▷ (\\([a-zA-Z0-9]+\\))\\." in
  if Str.string_match new_channel_pattern str 0 then
    let c = Str.matched_group 1 str in
    NewChannel c
  else if Str.string_match send_pattern str 0 then
    let a = Str.matched_group 1 str in
    let i = Str.matched_group 2 str in
    Send (a, i)
  else if Str.string_match receive_pattern str 0 then
    let c = Str.matched_group 1 str in
    let x = Str.matched_group 2 str in
    Receive (c, x)
  else if Str.string_match left_combine_pattern str 0 then
    let alpha = Str.matched_group 1 str in
    let k = Str.matched_group 2 str in
    LeftCombine (alpha, k)
  else if Str.string_match right_combine_pattern str 0 then
    let c = Str.matched_group 1 str in
    let x = Str.matched_group 2 str in
    RightCombine (c, x)
  else
    failwith ("Invalid action format: " ^ str)

(* Parsing Processes *)
let rec parse_process str =
  let null_process_pattern = Str.regexp "^0$" in
  let action_process_pattern = Str.regexp "^\\(.+\\) \\(.+\\)$" in
  let choice_pattern = Str.regexp "^\\(.+\\) + \\(.+\\)$" in
  let replication_pattern = Str.regexp "^!\\(.+\\)$" in
  if Str.string_match null_process_pattern str 0 then
    NullProcess
  else if Str.string_match action_process_pattern str 0 then
    let action_str = Str.matched_group 1 str in
    let process_str = Str.matched_group 2 str in
    print_endline ("Parsing action: " ^ action_str);
    print_endline ("Parsing process: " ^ process_str);
    let action = parse_action action_str in
    let process = parse_process process_str in
    ActionProcess (action, process)
  else if Str.string_match choice_pattern str 0 then
    let p1_str = Str.matched_group 1 str in
    let p2_str = Str.matched_group 2 str in
    print_endline ("Parsing choice left: " ^ p1_str);
    print_endline ("Parsing choice right: " ^ p2_str);
    let p1 = parse_process p1_str in
    let p2 = parse_process p2_str in
    Choice (p1, p2)
  else if Str.string_match replication_pattern str 0 then
    let p_str = Str.matched_group 1 str in
    print_endline ("Parsing replication: " ^ p_str);
    let p = parse_process p_str in
    Replication p
  else
    failwith ("Invalid process format: " ^ str)

(* Parsing Molecules *)
and parse_molecule str =
  let null_molecule_pattern = Str.regexp "NullMolecule" in
  let process_molecule_pattern = Str.regexp "ProcessMolecule(\\(.+\\))" in
  let resource_molecule_pattern = Str.regexp "ResourceMolecule(\\(.+\\))" in
  if Str.string_match null_molecule_pattern str 0 then
    NullMolecule
  else if Str.string_match process_molecule_pattern str 0 then
    let process_str = Str.matched_group 1 str in
    print_endline ("Parsing process molecule: " ^ process_str);
    let process = parse_process process_str in
    ProcessMolecule process
  else if Str.string_match resource_molecule_pattern str 0 then
    let resource_str = Str.matched_group 1 str in
    print_endline ("Parsing resource molecule: " ^ resource_str);
    let resource = parse_resource resource_str in
    ResourceMolecule resource
  else
    failwith ("Invalid molecule format: " ^ str)

(* Parsing Membranes *)
let rec parse_membrane str =
  let null_membrane_pattern = Str.regexp "NullMembrane" in
  let molecule_membrane_pattern = Str.regexp "MoleculeMembrane(\\(.+\\))" in
  let airlocked_membrane_pattern = Str.regexp "AirlockedMembrane(\\(.+\\), \\(.+\\), \\(.+\\))" in
  if Str.string_match null_membrane_pattern str 0 then
    NullMembrane
  else if Str.string_match molecule_membrane_pattern str 0 then
    let molecules_str = Str.matched_group 1 str in
    print_endline ("Parsing molecule membrane: " ^ molecules_str);
    let molecules = parse_molecule_list molecules_str in
    MoleculeMembrane molecules
  else if Str.string_match airlocked_membrane_pattern str 0 then
    let p_str = Str.matched_group 1 str in
    let r_str = Str.matched_group 2 str in
    let q_str = Str.matched_group 3 str in
    print_endline ("Parsing airlocked membrane P: " ^ p_str);
    print_endline ("Parsing airlocked membrane R: " ^ r_str);
    print_endline ("Parsing airlocked membrane Q: " ^ q_str);
    let p = parse_membrane p_str in
    let r = parse_resource r_str in
    let q = parse_membrane q_str in
    AirlockedMembrane (p, r, q)
  else
    failwith ("Invalid membrane format: " ^ str)

and parse_molecule_list str =
  let molecules = Str.split (Str.regexp ", ") str in
  List.map parse_molecule molecules

(* Equivalence Checker Functions *)

let rec equiv_resource res1 res2 = match (res1, res2) with
  | (NullResource, NullResource) -> true
  | (SimpleResource (alpha1, mu1), SimpleResource (alpha2, mu2)) -> 
      alpha1 = alpha2 && mu1 = mu2
  | (CombinedResource (alpha1, r1a, r1b), CombinedResource (alpha2, r2a, r2b)) ->
      alpha1 = alpha2 && equiv_resource r1a r2a && equiv_resource r1b r2b
  | (MeetOperation (r1a, r1b), MeetOperation (r2a, r2b)) ->
      (equiv_resource r1a r2a && equiv_resource r1b r2b) ||
      (equiv_resource r1a r2b && equiv_resource r1b r2a) (* commutative property *)
  | _ -> false

let rec equiv_process proc1 proc2 = match (proc1, proc2) with
  | (NullProcess, NullProcess) -> true
  | (ActionProcess (a1, p1), ActionProcess (a2, p2)) -> 
      equiv_action a1 a2 && equiv_process p1 p2
  | (Choice (p1a, p1b), Choice (p2a, p2b)) ->
      (equiv_process p1a p2a && equiv_process p1b p2b) ||
      (equiv_process p1a p2b && equiv_process p1b p2a) (* commutative property *)
  | (Replication p1, Replication p2) -> equiv_process p1 p2
  | _ -> false

and equiv_action a1 a2 = match (a1, a2) with
  | (NewChannel c1, NewChannel c2) -> c1 = c2
  | (Send (a1, i1), Send (a2, i2)) -> a1 = a2 && i1 = i2
  | (Receive (c1, x1), Receive (c2, x2)) -> c1 = c2 && x1 = x2
  | (LeftCombine (alpha1, mu1), LeftCombine (alpha2, mu2)) -> alpha1 = alpha2 && mu1 = mu2
  | (RightCombine (c1, x1), RightCombine (c2, x2)) -> c1 = c2 && x1 = x2
  | _ -> false

let equiv_molecule mol1 mol2 = match (mol1, mol2) with
  | (NullMolecule, NullMolecule) -> true
  | (ProcessMolecule p1, ProcessMolecule p2) -> equiv_process p1 p2
  | (ResourceMolecule r1, ResourceMolecule r2) -> equiv_resource r1 r2
  | _ -> false

let rec equiv_membrane mem1 mem2 = match (mem1, mem2) with
  | (NullMembrane, NullMembrane) -> true
  | (MoleculeMembrane m1, MoleculeMembrane m2) -> equiv_molecule_list m1 m2
  | (AirlockedMembrane (p1, r1, q1), AirlockedMembrane (p2, r2, q2)) ->
      equiv_membrane p1 p2 && equiv_resource r1 r2 && equiv_membrane q1 q2
  | _ -> false

and equiv_molecule_list m1 m2 = 
  try List.for_all2 equiv_molecule m1 m2
  with Invalid_argument _ -> false

(* Transition Rule for Channel Creation *)

let transition_channel_creation mem1 mem2 =
  match (mem1, mem2) with
  | (MoleculeMembrane ((ProcessMolecule (ActionProcess (NewChannel c1, p1))) :: ResourceMolecule NullResource :: ml1),
     MoleculeMembrane ((ProcessMolecule (ActionProcess (NewChannel c2, p2))) :: ResourceMolecule NullResource :: ml2))
    when c1 = c2 ->
    let new_res = SimpleResource (c1, "◦") in
    AirlockedMembrane (
      MoleculeMembrane (ProcessMolecule p1 :: ResourceMolecule new_res :: ml1),
      new_res,
      MoleculeMembrane (ProcessMolecule p2 :: ResourceMolecule new_res :: ml2)
    )
  | _ -> mem1 (* Return the original membrane if the pattern doesn't match *)

(* Transition Rule for QLocal *)

let transition_qlocal mem =
  match mem with
  | MoleculeMembrane ((ProcessMolecule (ActionProcess (LeftCombine (alpha, q), p))) ::
                      ResourceMolecule (SimpleResource (alpha', q')) :: ml) when alpha = alpha' ->
    let new_process = ActionProcess (LeftCombine (alpha, q ^ "." ^ q'), p) in
    MoleculeMembrane (ProcessMolecule new_process :: ResourceMolecule (SimpleResource (alpha, q')) :: ml)
  | _ -> mem

(* Transition Rule for CLocal *)

let transition_clocal mem =
  match mem with
  | MoleculeMembrane ((ProcessMolecule (ActionProcess (Receive (c, x), p))) ::
                      ResourceMolecule (SimpleResource (c', i)) :: ml) when c = c' ->
    let new_process = substitute x i p in
    MoleculeMembrane (ProcessMolecule new_process :: ml)
  | _ -> mem

(* Transition Rule for Encode *)

let transition_encode mem =
  match mem with
  | MoleculeMembrane ((ProcessMolecule (ActionProcess (LeftCombine (alpha, mu1), p))) ::
                      ResourceMolecule (SimpleResource (alpha', mu2)) :: ml) when alpha = alpha' ->
    let new_resource = meet_operation (SimpleResource (alpha, mu1)) (SimpleResource (alpha, mu2)) in
    MoleculeMembrane (ProcessMolecule p :: ResourceMolecule new_resource :: ml)
  | _ -> mem

(* Transition Rule for Decode *)

let transition_decode left_mem right_mem channel_content =
  match (left_mem, right_mem, channel_content) with
  | (MoleculeMembrane ((ProcessMolecule (ActionProcess (RightCombine (c, x), r))) :: ml1),
     MoleculeMembrane ((ProcessMolecule (ActionProcess (Receive (c', y), t))) :: ResourceMolecule (SimpleResource (c'', mu)) :: ml2),
     SimpleResource (c''', mu')) when c = c' && c' = c'' && c'' = c''' ->
    let new_r = substitute x (c ^ "." ^ mu') r in
    let new_t = substitute y c t in
    let new_resource = SimpleResource (c, "†" ^ mu') in
    (MoleculeMembrane (ProcessMolecule new_r :: ml1),
     MoleculeMembrane (ProcessMolecule new_t :: ResourceMolecule new_resource :: ml2))
  | _ -> (left_mem, right_mem)

(* Unit Test for Transition Rules *)

let test_transitions () =
  (* Initial Membranes for testing *)
  let qlocal_mem = MoleculeMembrane [
    ProcessMolecule (ActionProcess (LeftCombine ("c", "alpha"), NullProcess));
    ResourceMolecule (SimpleResource ("c", "q"));
    ProcessMolecule NullProcess
  ] in

  let clocal_mem = MoleculeMembrane [
    ProcessMolecule (ActionProcess (Receive ("c", "x"), NullProcess));
    ResourceMolecule (SimpleResource ("c", "i"));
    ProcessMolecule NullProcess
  ] in

  let encode_mem = MoleculeMembrane [
    ProcessMolecule (ActionProcess (LeftCombine ("alpha", "mu1"), NullProcess));
    ResourceMolecule (SimpleResource ("alpha", "mu2"));
    ProcessMolecule NullProcess
  ] in

  let left_decode_mem = MoleculeMembrane [
    ProcessMolecule (ActionProcess (RightCombine ("c", "x"), NullProcess));
    ProcessMolecule NullProcess
  ] in

  let right_decode_mem = MoleculeMembrane [
    ProcessMolecule (ActionProcess (Receive ("c", "y"), NullProcess));
    ResourceMolecule (SimpleResource ("c", "mu"));
    ProcessMolecule NullProcess
  ] in

  let channel_content = SimpleResource ("c", "mu") in

  (* Apply transitions *)
  let qlocal_result = transition_qlocal qlocal_mem in
  let clocal_result = transition_clocal clocal_mem in
  let encode_result = transition_encode encode_mem in
  let (left_decode_result, right_decode_result) = transition_decode left_decode_mem right_decode_mem channel_content in

  (* Print results *)
  print_string "Initial QLocal membrane:\n";
  print_membrane qlocal_mem; print_string "\nAfter QLocal transition:\n";
  print_membrane qlocal_result; print_string "\n\n";

  print_string "Initial CLocal membrane:\n";
  print_membrane clocal_mem; print_string "\nAfter CLocal transition:\n";
  print_membrane clocal_result; print_string "\n\n";

  print_string "Initial Encode membrane:\n";
  print_membrane encode_mem; print_string "\nAfter Encode transition:\n";
  print_membrane encode_result; print_string "\n\n";

  print_string "Initial Left Decode membrane:\n";
  print_membrane left_decode_mem; print_string "\nInitial Right Decode membrane:\n";
  print_membrane right_decode_mem; print_string "\nAfter Decode transition:\n";
  print_membrane left_decode_result; print_string "\n";
  print_membrane right_decode_result; print_string "\n"

let () = test_transitions ()
