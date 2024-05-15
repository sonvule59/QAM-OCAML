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

(* Define complex numbers for quantum computations *)
type complex = { re: float; im: float }

(* Define quantum gates and their effects on qubits *)
type quantum_gate =
  | Identity of int
  | PauliX of int
  | PauliZ of int
  | Hadamard of int
  | CNOT of int * int
  | TGate of int
  | CustomGate of complex array array

(* Define the quantum state based on the number of qubits and their states *)
type quantum_state = {
  num_qubits: int;
  state_vector: complex array;
}
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
  | Replication of process              (* !R *)
  | If of boolean_expr * process * process  (* If condition, then process, else process *)
  | While of boolean_expr * process  (* While condition holds, perform process *)

type molecule =
  | NullMolecule
  | ProcessMolecule of process
  | ResourceMolecule of resource

type membrane =
  | NullMembrane
  | MoleculeMembrane of molecule list         (* {|M|} *)
  | AirlockedMembrane of membrane * resource * membrane  (* P |[ϕ]| Q *)


  (* Semantic Rules and the function for membrane*)
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

let () =
let results = execute_process initial_process in
print_results results

 (*QAM Sytax Extension  *) 
(* type locations = 
type success_rate =  *)
