(* qam — command-line entry point for the QAM toolchain.
   Subcommands: compile (QAM -> OpenQASM), simulate (compile + built-in
   statevector simulator), repl (interactive reduction/equivalence). *)

let read_file (path : string) : string =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let parse_file (path : string) : Ast.membrane list =
  match Interpreter.parse_membranes_from_string (read_file path) with
  | Ok membranes -> membranes
  | Error e ->
      Interpreter.print_parse_error e;
      exit 1

let cmd_compile path = print_string (Compile.compile_to_qasm (parse_file path))

let print_branch (br : Sim.branch) =
  let cregs = List.sort compare br.Sim.cregs in
  let creg_s =
    if cregs = [] then "(no measurements)"
    else String.concat " " (List.map (fun (n, v) -> Printf.sprintf "%s=%d" n v) cregs)
  in
  Printf.printf "branch %s  p=%.4f\n" creg_s (Sim.norm2 br.Sim.amp);
  let dim = Array.length br.Sim.amp in
  let nq =
    let rec log2 n acc = if n <= 1 then acc else log2 (n / 2) (acc + 1) in
    log2 dim 0
  in
  Array.iteri
    (fun idx c ->
      if Complex.norm c > 1e-9 then
        let bits =
          String.init nq (fun i ->
              if idx land (1 lsl (nq - 1 - i)) <> 0 then '1' else '0')
        in
        Printf.printf "  %+.4f%+.4fi |%s>\n" c.Complex.re c.Complex.im bits)
    br.Sim.amp

let cmd_simulate path =
  let qasm = Compile.compile_to_qasm (parse_file path) in
  let branches = Sim.run qasm in
  Printf.printf "%d branch(es); qubits start at |0...0>:\n" (List.length branches);
  List.iter print_branch branches

let usage () =
  print_endline "qam - Quantum Abstract Machine toolchain";
  print_endline "";
  print_endline "Usage:";
  print_endline "  qam compile FILE.qam    compile a QAM configuration to OpenQASM 2.0";
  print_endline "  qam simulate FILE.qam   compile, then run on the built-in statevector simulator";
  print_endline "  qam repl                interactive reduction / equivalence REPL"

let () =
  match Array.to_list Sys.argv with
  | [ _; "compile"; path ] -> cmd_compile path
  | [ _; "simulate"; path ] -> cmd_simulate path
  | [ _; "repl" ] ->
      print_endline "Welcome to the interactive interpreter!";
      Interpreter.interactive_prompt []
  | _ -> usage ()
