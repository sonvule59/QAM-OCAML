(* Define the resource and action types *)
type resource =
  | SimpleResource of string * string   (* α.μ *)
  | NullResource                        (* ◦ *)

type action =
  | NewChannel of string                (* ν c. *)
  | Send of string * string             (* a!ι. *)
  | Receive of string * string          (* δ?(x). *)
  | Move of string                      (* Move to a location *)
  | LeftCombine of string * string      (* α ◁ κ. *) (* This is encoding *)
  | RightCombine of string * string     (* c ▷ (x). *) (* This is decoding *)

type process =
  | NullProcess                         (* 0 *)
  | ActionProcess of action * process   (* AR *)
  | Parallel of process list            (* Parallel composition *)

type molecule =
  | ProcessMolecule of process
  | ResourceMolecule of resource

type membrane =
  | MoleculeMembrane of molecule list
  | AirlockedMembrane of membrane * resource * membrane  (* P |[ϕ]| Q *)

type membrane_conf = {
  processes: process list;
  locked: bool;
}

type configuration = membrane_conf list

(* Function to combine two resources *)
let combine_resources r1 r2 =
  match (r1, r2) with
  | (SimpleResource (alpha1, mu1), SimpleResource (alpha2, mu2)) ->
    SimpleResource (alpha1 ^ alpha2, mu1 ^ mu2)
  | _ -> NullResource

(* Transition Function *)
let rec step_process process actor =
  match process with
  | NullProcess -> NullProcess
  | ActionProcess (action, next) -> 
    (match action with
     | NewChannel c -> 
       Printf.printf "%s: Creating new channel: %s\n" actor c;
       ActionProcess (action, step_process next actor)
     | Send (a, b) -> 
       Printf.printf "%s: Sending message '%s' on channel: %s\n" actor b a;
       ActionProcess (action, step_process next actor)
     | Receive (a, b) -> 
       Printf.printf "%s: Receiving message on channel '%s' into variable: %s\n" actor a b;
       ActionProcess (action, step_process next actor)
     | LeftCombine (a, b) -> 
       Printf.printf "%s: Encoding with left combine: %s ◁ %s\n" actor a b;
       ActionProcess (action, step_process next actor)
     | RightCombine (a, b) -> 
       Printf.printf "%s: Decoding with right combine: %s ▷ %s\n" actor a b;
       ActionProcess (action, step_process next actor)
     | Move loc -> 
       Printf.printf "%s: Moving to location: %s\n" actor loc;
       ActionProcess (action, step_process next actor))
  | Parallel processes -> Parallel (List.map (fun p -> step_process p actor) processes)

(* Function to execute all processes in a membrane *)
let step_membrane membrane actor =
  { membrane with processes = List.map (fun p -> step_process p actor) membrane.processes }

(* Function to execute a configuration *)
let step_configuration config =
  List.mapi (fun i mem -> 
    if i = 0 then step_membrane mem "Alice" else step_membrane mem "Bob"
  ) config

(* Example 6: Quantum Teleportation Decoding Steps *)
let quantum_teleportation_decoding_example () =
  let alice_process = Parallel [
    ActionProcess (RightCombine ("c", "x"), 
      ActionProcess (Send ("a", "x"), NullProcess))
  ] in
  
  let bob_process = Parallel [
    ActionProcess (Receive ("c", "u"), 
      ActionProcess (LeftCombine ("u", "z"), NullProcess))
  ] in
  
  let alice_membrane = {
    processes = [alice_process];
    locked = false;
  } in

  let bob_membrane = {
    processes = [bob_process];
    locked = false;
  } in

  let configuration = [alice_membrane; bob_membrane] in

  configuration

let print_configuration config label =
  Printf.printf "%s Configuration:\n" label;
  List.iteri (fun i mem -> 
    if i = 0 then Printf.printf "  Alice's actions:\n" else Printf.printf "  Bob's actions:\n";
    List.iter (fun proc ->
      match proc with
      | Parallel actions -> 
        List.iter (fun action -> 
          match action with
          | ActionProcess (act, _) -> 
            (match act with
             | NewChannel c -> Printf.printf "    [Action] NewChannel %s\n" c
             | Send (a, b) -> Printf.printf "    [Action] Send %s %s\n" a b
             | Receive (a, b) -> Printf.printf "    [Action] Receive %s %s\n" a b
             | LeftCombine (a, b) -> Printf.printf "    [Action] LeftCombine %s %s\n" a b
             | RightCombine (a, b) -> Printf.printf "    [Action] RightCombine %s %s\n" a b
             | Move loc -> Printf.printf "    [Action] Move to %s\n" loc)
          | _ -> ()) actions
      | _ -> ()) mem.processes) config;
  Printf.printf "\n"

let () =
  let config = quantum_teleportation_decoding_example () in
  print_configuration config "Initial";

  let next_config = step_configuration config in
  print_configuration next_config "Next";
