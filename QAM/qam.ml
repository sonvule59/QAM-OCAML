(* This is a syntax implemetation of Quantum Abstract Machine
https://inspirehep.net/literature/2760484   
@Author: Son Vu
@Date: Spring 2024
@Note: Part of Masters' Thesis for Computer Science, Iowa State University
*)

(* Syntax Definition *)
type resource =
  | SimpleResource of string * string   (* α.μ *)
  | NullResource                         (* ◦ *)

type action =
  | NewChannel of string                (* ν c. *)
  | Send of string * string             (* a!ι. *)
  | Receive of string * string          (* δ?(x). *)
  | LeftCombine of string * string      (* α ◁ κ. *)
  | RightCombine of string * string     (* c ▷ (x). *)

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
  | SimpleMembrane of molecule          (* {|M|} *)
  | CompositeMembrane of membrane * resource * membrane  (* P |[ϕ]| Q *)


  (* Semantic Rules *)
let rec transform_membrane = function
(* Split rule *)
| SimpleMembrane ms ->  (* Split rule *)
      List.map (fun m -> SimpleMembrane [m]) ms 
(* | SimpleMembrane (MultipleMolecules ms) ->  
    CompositeMembrane (List.map (fun m -> SimpleMembrane m) ms |> List.hd, 
                       CompositeMembrane (List.map (fun m -> SimpleMembrane m) ms |> List.tl)) *)

(* CL and CR rules *)
(* | SimpleMembrane (SingleMolecule (Choice (r, t), res)) ->  
    if Random.bool () then SimpleMembrane (SingleMolecule (r, res))
    else SimpleMembrane (SingleMolecule (t, res))
(* MT rule *)
| SimpleMembrane (SingleMolecule (Replication r, res)) ->  
    CompositeMembrane (SimpleMembrane (SingleMolecule (r, res)), SimpleMembrane (SingleMolecule (Replication r, res)))
(* ID1 *)
| SimpleMembrane NullMolecule -> NullMembrane  
(* ID2 *)
| SimpleMembrane (ProcessMolecule NullProcess) -> NullMembrane  
(* NT *)
| SimpleMembrane (ProcessMolecule (Replication NullProcess)) -> NullMembrane  
(* Decohere *)
| SimpleMembrane (ResourceMolecule NullResource) -> NullMembrane  
(* MT *)
| SimpleMembrane (ProcessMolecule (Replication p)) -> 
    CompositeMembrane (SimpleMembrane (ProcessMolecule p), SimpleMembrane (ProcessMolecule (Replication p)))  

| membrane -> membrane *)

 (* ID1 *)
| SimpleMembrane [] -> [NullMembrane] 
 (* Single molecule handling *)
  | SimpleMembrane [m] -> begin match m with 

  (* ID1, ID2 *)
      | NullMolecule | ProcessMolecule NullProcess -> [NullMembrane]
      (* CL and CR rules *)
      | SimpleMembrane (SingleMolecule (Choice (r, t), res)) ->  
        if Random.bool () then SimpleMembrane (SingleMolecule (r, res))
        else SimpleMembrane (SingleMolecule (t, res))
        (* NT *)
      | ProcessMolecule (Replication NullProcess) -> [NullMembrane]  
       (* MT *)
      | ProcessMolecule (Replication p) -> [SimpleMembrane [ProcessMolecule p]; SimpleMembrane [ProcessMolecule (Replication p)]] 
      (* Decoherence *)
      | ResourceMolecule NullResource -> [NullMembrane]  
      | _ -> [SimpleMembrane [m]]
    end
    (* Split *)
  | SimpleMembrane ms when List.length ms > 1 -> List.map (fun m -> SimpleMembrane [m]) ms  
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



 (*QAM Sytax Extension  *) 
(* type locations = 
type success_rate =  *)
