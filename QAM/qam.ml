(* This is a syntax implemetation of Quantum Abstract Machine
https://inspirehep.net/literature/2760484   
@Author: Son Vu
@Date: Spring 2024
@Note: The Project is Part of Masters' Thesis for Computer Science, Iowa State University
*)
(****************************************************************************************************
The operation design

  The syntax is created with the idea that 
  processes and quantum resources coexist as molecules in a membrane 
  (based on the CHAM paper). There are two kinds of membranes: molecule and airlocked

  Resources (ϕ):
    `α.μ`: Represents a resource labeled with an action (`α`) and a molecule (`μ`). 
          The action specifies the type of interaction possible with the resource.
    `◦`: Represents the empty resource (no interaction possible).

  Actions (A):
    `ν c.`: Represents a private communication action on channel `c`.
    `a!ι.`: Represents sending a message `ι` on channel `a`.
    `δ?(x).`: Represents receiving a message and storing it in variable `x` from channel `δ`.
    `α ◁ κ.`: Represents consuming a resource with action `α` and creating a new resource `κ`. 
    `c ▷ (x).`: Represents offering a resource with action `α` based on the value of variable `x`.

  Processes (R, T):
    `0`: Represents the inactive process.
    `AR`: Represents a process performing action `A` followed by process `R`.
    `R + T`: Represents non-deterministic choice between process `R` and process `T`.
    `!R`: Represents replicated process, continuously spawning new instances of `R`.

  Molecules (M, N):
    `R`: A process can be a molecule.
    `ϕ`: A resource can be a molecule.

  Membranes (P, Q):
    `{|M|}`: Represents a membrane containing molecule `M`.
    `P | Q`: Represents parallel composition of membranes `P` and `Q`.
    `[ϕ]`: Represents a channel labeled with resource `ϕ`.
    `P ◦ Q`: Represents sequential composition of membranes `P` and `Q` 
(communication happens only through channels).

*****************************************************************************************************)
(* Type definition *)

(* Define Boolean type *)
type boolean_expr =
  | True
  | False
  | QuantumMeasurementResult of int  
  | ClassicalComparison of string * string 

(*Define Syntax *)
type resource =
  | SimpleResource of string * string   (* α.μ *)
  | NullResource                         (* ◦ *)

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
  | Message of name * message * process
  | Replication of process              (* !R *)
  | If of boolean_expr * process * process  (* If condition, then process, else process *)
  | While of boolean_expr * process  (* While condition holds, perform process *)
  | Conditional of (process -> bool) * process * process

type molecule =
  | NullMolecule
  | ProcessMolecule of process
  | ResourceMolecule of resource

type membrane =
  | NullMembrane
  | MoleculeMembrane of molecule list         (* {|M|} *)
  | AirlockedMembrane of membrane * resource * membrane  (* P |[ϕ]| Q *)
(* 
type membrane = {
  processes: process list;
  location: location;
} *)

let rec simulate_process process =
  match process with
  | NullProcess -> "End of process"
  | Operation (action, next) ->
    begin
      match action with
      | Send (channel, message) -> Printf.sprintf "Sending %s on channel %s\n" (match message with QuantumData q -> q | ClassicalData c -> c) channel
      | Receive (channel, message) -> Printf.sprintf "Receiving %s on channel %s\n" (match message with QuantumData q -> q | ClassicalData c -> c) channel
      | Move location -> Printf.sprintf "Moving to location %s\n" location
    end ^ simulate_process next
  | Message (channel, message, next) ->
    Printf.sprintf "Handling message on channel %s\n" channel ^ simulate_process next
  | Transition (state, new_location, next) ->
    Printf.sprintf "Transitioning from %s to %s\n" state.location new_location ^ simulate_process next
  | Conditional (pred, p1, p2) -> (*Note that Conditional represents both If and While *)
    if pred NullProcess then simulate_process p1 else simulate_process p2
  | Parallel (p1, p2) ->
    simulate_process p1 ^ simulate_process p2

  (* Function for membrane*)
let rec transform_membrane = function
(* Split rule *)
| MoleculeMembrane ms ->  (* Split rule *)
      List.map (fun m -> MoleculeMembrane [m]) ms 
 (* ID1 *)
| MoleculeMembrane [] -> [NullMembrane] 
 (* Single molecule handling *)
  | MoleculeMembrane [m] -> begin match m with 

(* ID1, ID2 *)
    | NullMolecule | ProcessMolecule NullProcess -> [NullMembrane]
    (* CL and CR rules *)
    | MoleculeMembrane (SingleMolecule (Choice (r, t), res)) ->  
      if Random.bool () then MoleculeMembrane (SingleMolecule (r, res))
      else MoleculeMembrane (SingleMolecule (t, res))
      (* NT *)
    | ProcessMolecule (Replication NullProcess) -> [NullMembrane]  
      (* MT *)
    | ProcessMolecule (Replication p) -> [MoleculeMembrane [ProcessMolecule p]; MoleculeMembrane [ProcessMolecule (Replication p)]] 
    (* Decoherence *)
    | ResourceMolecule NullResource -> [NullMembrane]  
    | _ -> [MoleculeMembrane [m]]
  end
    (* Split *)
  | MoleculeMembrane ms when List.length ms > 1 -> List.map (fun m -> MoleculeMembrane [m]) ms  
    (* Default case *)
  | membrane -> [membrane]

(*Implementation of the Semantics *) 
let rec evaluate_process = function
  | NullProcess -> ()
  | ActionProcess (action, next_process) ->
      (match action with
      | NewChannel c -> print_endline ("New channel created: " ^ c)
      | Send (a, i) -> print_endline ("Sending on channel " ^ a ^ ": " ^ i)
      | _ -> ());
      evaluate_process next_process
  | Choice (p1, p2) ->
      (* Simple nondeterministic choice handling, could be extended 
         This is the extension *)
      if Random.bool () then evaluate_process p1 else evaluate_process p2
  | Replication p ->
      (* Handling replication *)
      evaluate_process p;
      evaluate_process (Replication p)

(* let () = *)
let results = execute_process initial_process in
print_results results

(* IMPLEMENT the Parser
    Utility function to trim whitespace from a string *)
let trim str = 
  let open String in
  sub str (find_first_not_of " \t\n\r" str) 
    ((find_last_not_of " \t\n\r" str) - (find_first_not_of " \t\n\r" str) + 1)

(* Parse a single action from a string *)
let parse_action input =
  let parts = String.split_on_char ' ' (trim input) in
  match parts with
  | "send" :: rest -> Send (String.concat " " rest)
  | "receive" :: rest -> Receive (String.concat " " rest)
  | _ -> failwith "Unknown action"

(* Recursive function to parse a process *)
let rec parse_process input =
  let input = trim input in
  if String.sub input 0 6 = "choose" then
    let inner = String.sub input 7 (String.length input - 8) in
    let parts = String.split_on_char ',' inner in
    if List.length parts <> 2 then failwith "Fail"
    else Choice (parse_process (List.nth parts 0), parse_process (List.nth parts 1))
  else
    let actions = String.split_on_char ';' input in
    List.fold_right (fun act acc ->
      ActionProcess (parse_action act, acc)
    ) actions NullProcess

