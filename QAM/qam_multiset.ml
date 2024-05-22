type action =
  | Send of string * string
  | Receive of string * string

type process =
  | NullProcess
  | ActionProcess of action * process 
  | Conditional of bool * process * process  (* represents Trans *)
  | Parallel of process list
  | Replication of process
  | Choice of process * process

type membrane = {
  processes: process list;
  location: string; (*This will be {|M|}, the fixed location in 4.1 *)
  locked: bool;
}
type configuration = membrane list

let transition membrane =
  { membrane with processes = List.concat_map (fun p ->
    match p with
    | ActionProcess (Send (ch, msg), ps) -> 
        [ActionProcess (Receive (ch, msg), ps)]  (* Simplify transition *)
    | _ -> [p]
  ) membrane.processes }
    (* processes = List.map (fun p ->
      match p with
      | ActionProcess (Send (ch, msg), ps) -> 
          [ActionProcess (Receive (ch, msg), ps)]  (* Simplify transition *)
      | _ -> [p]
    ) membrane.processes } *)

(* Function to normalize a list to its canonical form *)
let rec normalize_process process =
  match process with
  | Parallel processes ->
    Parallel (List.sort compare (List.map normalize_process processes))
  | ActionProcess (action, next) -> ActionProcess (action, normalize_process next)
  | Replication proc -> Replication (normalize_process proc)
  | _ -> process

(* Dummy functions for loc and is_q *)
let loc _ _ = ("loc1", "loc2")
let is_q _ = true

(* Implements Trans rule *)
let trans_rule process =
  match process with
  | Conditional (predicate, p1, p2) when predicate -> p1
  | _ -> process

(* Implements Chan rule *)
let chan_rule process =
  match process with
  | ActionProcess (CreateChannel (chan, rate), next) ->
    Printf.printf "Channel %s created with success rate %.2f\n" chan rate;
    next
  | _ -> process

(* Implements Msg rule *)
let msg_rule process =
  match process with
  | ActionProcess (Send (chan, message), next) ->
    Printf.printf "Message '%s' sent on channel %s\n" message chan;
    next
  | _ -> process

let compare_process p1 p2 =
  match (p1, p2) with
  | ActionProcess (a1, _), ActionProcess (a2, _) -> compare a1 a2  (* Simplify comparison *)

 (*QAM Sytax Extension  *) 
(* type channel = string *)
let rec traces process =
  match process with
  | NullProcess -> [[]]  (* Base case: return a list with an empty trace *)
  | ActionProcess (action, next) -> 
    List.map (fun trace -> action :: trace) (traces next)
  | Parallel (p1, p2) ->
    let traces1 = traces p1 in
    let traces2 = traces p2 in
    List.flatten (List.map (fun trace1 -> List.map (fun trace2 -> trace1 @ trace2) traces2) traces1)
  | Choice (p1, p2) ->
    traces p1 @ traces p2

(* Definition of Equivalence Checker *)
let equivalent p1 p2 =
  let traces1 = traces p1 in
  let traces2 = traces p2 in
  List.for_all (fun trace1 -> List.exists (fun trace2 -> trace1 = trace2) traces2) traces1 &&
  List.for_all (fun trace2 -> List.exists (fun trace1 -> trace1 = trace2) traces1) traces2

let process1 = Parallel (Action (Send ("a", "hello"), NullProcess), Action (Receive ("b", "world"), NullProcess))
let process2 = Parallel (Action (Receive ("b", "world"), NullProcess), Action (Send ("a", "hello"), NullProcess))

let () =
  if equivalent process1 process2 then
    print_endline "Process1 and Process2 are equivalent."
  else
    print_endline "Process1 and Process2 are not equivalent."


let sort_membrane membrane =
  { membrane with processes = List.sort compare_process membrane.processes }

  let rec main_loop configuration =
    Printf.printf "Current configuration: %s\n" (string_of_configuration configuration);
    Printf.printf "Enter your equivalence configuration or 'exit' to quit:\n";
    let input = read_line () in
    if input = "exit" then
      Printf.printf "Exiting...\n"
    else
      let user_configuration = parse_configuration input in  (* Assume function to parse user input into configuration *)
      let new_configuration = List.map transition configuration |> List.map sort_membrane in
      if are_equivalent new_configuration user_configuration then
        Printf.printf "User configuration is equivalent.\n"
      else
        Printf.printf "User configuration is not equivalent.\n";
      main_loop new_configuration  (* Continue with the new configuration *)
  
  let () = main_loop initial_configuration  (* Start with an initial configuration *)
  