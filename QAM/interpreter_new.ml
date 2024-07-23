(* Define the types *)
type resource =
  | SimpleResource of string * string   (* α.μ *)
  | CombinedResource of string * resource * resource (* α.(μ1 ⊙ μ2) *)
  | NullResource                        (* ◦ *)
  | MeetOperation of resource * resource  (* μ1 ⊙ μ2 *)

type action =
  | NewChannel of string                (* ν c. *)
  | Send of string * string             (* a!ι. *)
  | Receive of string * string          (* δ?(x). *)
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

(* Helper function to combine resources using the meet operation ⊙ *)
let meet_operation res1 res2 = MeetOperation (res1, res2)

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
      print_string (c ^ "?(x). "); print_process p
  | ActionProcess (NewChannel c, p) ->
      print_string ("ν " ^ c ^ ". "); print_process p
  | _ -> print_string "..."

(* Function to print the current state of a molecule *)
let print_molecule = function
  | NullMolecule -> print_string "NullMolecule"
  | ProcessMolecule p -> print_string "ProcessMolecule("; print_process p; print_string ")"
  | ResourceMolecule res -> print_string "ResourceMolecule("; print_resource res; print_string ")"
  | _ -> print_string "..."

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
      print_membrane q; 
      print_string ")"

(* Interpreter function to handle all the rules *)
let rec interpret mem = match mem with
  | NullMembrane -> NullMembrane
  | MoleculeMembrane molecules ->
    let rec process_molecules mols = match mols with
      | [] -> []
      | (ProcessMolecule p)::rest ->
        let resources = List.filter (function | ResourceMolecule _ -> true | _ -> false) rest in
        
        (* Apply QLocal rule *)
        let p = List.fold_left (fun acc res -> match (acc, res) with
          | (ActionProcess (LeftCombine (alpha, q), r), ResourceMolecule (SimpleResource (alpha', q'))) when alpha = alpha' ->
            let new_p = ActionProcess (LeftCombine (alpha, q'), r) in
            print_string "Applying QLocal: "; print_process acc; print_string " -> "; print_process new_p; print_newline ();
            new_p
          | _ -> acc) p resources in
        
        (* Apply CLocal rule *)
        let p = List.fold_left (fun acc res -> match (acc, res) with
          | (ActionProcess (Receive (c, x), r), ResourceMolecule (SimpleResource (c', i))) when c = c' ->
            let rec substitute var value = function
              | NullProcess -> NullProcess
              | ActionProcess (a, p) -> ActionProcess (a, substitute var value p)
              | Choice (p1, p2) -> Choice (substitute var value p1, substitute var value p2)
              | Replication p -> Replication (substitute var value p)
            in
            let new_p = substitute x i r in
            print_string "Applying CLocal: "; print_process acc; print_string " -> "; print_process new_p; print_newline ();
            new_p
          | _ -> acc) p resources in
        
        (* Apply Encode rule *)
        let (p, resources) = List.fold_left (fun (acc_p, acc_res) res -> match (acc_p, res) with
          | (ActionProcess (LeftCombine (alpha, mu1), r), ResourceMolecule (SimpleResource (alpha', mu2))) when alpha = alpha' ->
            let new_res = ResourceMolecule (meet_operation (SimpleResource (alpha, mu1)) (SimpleResource (alpha', mu2))) in
            print_string "Applying Encode: "; print_process acc_p; print_string " with "; print_molecule res; print_string " -> "; print_molecule new_res; print_newline ();
            (ActionProcess (LeftCombine (alpha, mu1), r), new_res :: acc_res)
          | _ -> (acc_p, res :: acc_res)) (p, []) resources in
        
        ProcessMolecule p :: resources @ process_molecules rest
      | m::rest -> m :: process_molecules rest
    in
    MoleculeMembrane (process_molecules molecules)
  | AirlockedMembrane (p, r, q) ->
    let new_p = interpret p in
    let new_q = interpret q in

    (* Apply Decode rule *)
    let (new_p, new_q) = match (new_p, new_q) with
      | (MoleculeMembrane mols_p, MoleculeMembrane mols_q) ->
        let rec apply_decode_rule mols_p mols_q acc_p acc_q = match (mols_p, mols_q) with
          | (ProcessMolecule (ActionProcess (RightCombine (c, x), rp)) :: rest_p,
             ProcessMolecule (ActionProcess (Receive (c', y), rq)) :: rest_q) when c = c' ->
            let new_p = ActionProcess (RightCombine (c, x), rp) in
            let new_q = ActionProcess (Receive (c', y), rq) in
            print_string "Applying Decode: "; 
            print_process new_p; print_string " and "; print_process new_q; 
            print_string " -> "; 
            print_process (ActionProcess (RightCombine (c, x), rp)); 
            print_string " and "; 
            print_process (ActionProcess (Receive (c', y), rq)); 
            print_newline ();
            (ProcessMolecule new_p :: acc_p, ProcessMolecule new_q :: acc_q)
          | (mp :: rest_p, mq :: rest_q) -> apply_decode_rule rest_p rest_q (mp :: acc_p) (mq :: acc_q)
          | ([], _) -> (List.rev acc_p, mols_q @ acc_q)
          | (_, []) -> (mols_p @ acc_p, List.rev acc_q)
        in
        let (final_p, final_q) = apply_decode_rule mols_p mols_q [] [] in
        (MoleculeMembrane final_p, MoleculeMembrane final_q)
      | _ -> (new_p, new_q)
    in

    (* Apply Cohere rule directly here *)
    let (new_p, new_q) = match (new_p, new_q) with
      | (MoleculeMembrane (ProcessMolecule (ActionProcess (NewChannel c, p1)) :: rest_p1), MoleculeMembrane (ProcessMolecule (ActionProcess (NewChannel c', p2)) :: rest_p2))
        when c = c' ->
        let blank_resources_p1 = List.filter (function | ResourceMolecule NullResource -> true | _ -> false) rest_p1 in
        let blank_resources_p2 = List.filter (function | ResourceMolecule NullResource -> true | _ -> false) rest_p2 in
        if List.length blank_resources_p1 > 0 && List.length blank_resources_p2 > 0 then
          let new_resources_p1 = List.map (fun _ -> ResourceMolecule (SimpleResource (c, "◦"))) blank_resources_p1 in
          let new_resources_p2 = List.map (fun _ -> ResourceMolecule (SimpleResource (c, "◦"))) blank_resources_p2 in
          print_string "Applying Cohere: ";
          print_process (ActionProcess (NewChannel c, p1)); print_string " and ";
          print_process (ActionProcess (NewChannel c', p2)); print_string " -> ";
          List.iter (fun res -> print_molecule res; print_string ", ") new_resources_p1;
          List.iter (fun res -> print_molecule res; print_string ", ") new_resources_p2;
          print_newline ();
          let merged_membrane = MoleculeMembrane (rest_p1 @ new_resources_p1 @ rest_p2 @ new_resources_p2) in
          (merged_membrane, MoleculeMembrane [])
        else (new_p, new_q)
      | _ -> (new_p, new_q)
    in
    AirlockedMembrane (new_p, r, new_q)

(* Example usage *)
let example_process_encode = ActionProcess (LeftCombine ("alpha", "mu1"), NullProcess)
let example_resource_encode = ResourceMolecule (SimpleResource ("alpha", "mu2"))

let example_process_decode = ActionProcess (RightCombine ("c", "x"), NullProcess)
let example_receive_decode = ActionProcess (Receive ("c", "y"), NullProcess)

let example_process_cohere1 = ActionProcess (NewChannel "c", NullProcess)
let example_process_cohere2 = ActionProcess (NewChannel "c", NullProcess)
let example_blank_resource1 = ResourceMolecule NullResource
let example_blank_resource2 = ResourceMolecule NullResource

let example_membrane = MoleculeMembrane [
  ProcessMolecule example_process_encode; 
  example_resource_encode; 
  ProcessMolecule example_process_decode; 
  ProcessMolecule example_receive_decode;
  ProcessMolecule example_process_cohere1;
  example_blank_resource1;
  ProcessMolecule example_process_cohere2;
  example_blank_resource2
]

let () =
  let new_membrane = interpret example_membrane in
  print_string "Final state: "; print_membrane new_membrane; print_newline ()
