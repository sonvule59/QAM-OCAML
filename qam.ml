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

(* run the static checker; on failure print positioned errors and exit *)
let check_or_exit (membranes : Ast.membrane list) =
  match Typecheck.check membranes with
  | Ok kinds -> kinds
  | Error errs ->
      List.iter
        (fun e -> prerr_endline ("Type error: " ^ Typecheck.string_of_error e))
        errs;
      exit 1

let cmd_check path =
  let membranes = parse_file path in
  let kinds = check_or_exit membranes in
  Printf.printf "OK: %d membrane(s)\n" (List.length membranes);
  let show = function Typecheck.KQuantum -> "quantum" | Typecheck.KClassical -> "classical" in
  Hashtbl.iter (fun c k -> Printf.printf "  channel %-8s %s\n" c (show k)) kinds

let cmd_compile ?(target = `Flat) path =
  let membranes = parse_file path in
  let _ = check_or_exit membranes in
  let circuit = Compile.lower membranes in
  print_string
    (match target with
    | `Flat -> Ir.to_qasm circuit
    | `Qasm3 -> Ir.to_qasm3 circuit
    | `Concurrent -> Ir.to_concurrent circuit)

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

(* explore the abstract machine's LTS to its terminal configurations *)
let cmd_run ?(depth = 32) path =
  let membranes = parse_file path in
  let _ = check_or_exit membranes in
  Printf.printf "initial : %s\n" (Semantics.string_of_config membranes);
  match Semantics.reachable_terminals ~depth membranes with
  | [] -> Printf.printf "no terminal configuration within %d steps\n" depth
  | terms ->
      List.iter
        (fun (t, tr) ->
          Printf.printf "terminal: %s\n  trace : [%s]\n"
            (Semantics.string_of_config t)
            (String.concat "; " tr))
        terms

let cmd_simulate ?(concurrent = false) path =
  let membranes = parse_file path in
  let _ = check_or_exit membranes in
  let branches =
    if concurrent then Sim.run_ir (Compile.lower membranes)
    else Sim.run (Compile.compile_to_qasm membranes)
  in
  Printf.printf "%d branch(es); qubits start at |0...0>%s:\n" (List.length branches)
    (if concurrent then " (concurrent runtime)" else "");
  List.iter print_branch branches

let usage () =
  print_endline "qam - Quantum Abstract Machine toolchain";
  print_endline "";
  print_endline "Usage:";
  print_endline "  qam check FILE.qam      typecheck a QAM configuration (Omega, well-formedness, no-cloning)";
  print_endline "  qam compile FILE.qam    typecheck, then compile to OpenQASM 2.0";
  print_endline "      --qasm3                 ... to OpenQASM 3.0 (bits, input parameters)";
  print_endline "      --concurrent            ... to the concurrent circuit (target b)";
  print_endline "  qam simulate FILE.qam   compile, then run on the built-in statevector simulator";
  print_endline "      --concurrent            ... on the concurrent runtime (blocking send/wait)";
  print_endline "  qam run FILE.qam [N]    execute the abstract machine (LTS) to its terminals (depth N)";
  print_endline "  qam repl                interactive reduction / equivalence REPL"

let () =
  match Array.to_list Sys.argv with
  | [ _; "check"; path ] -> cmd_check path
  | [ _; "run"; path ] -> cmd_run path
  | [ _; "run"; path; d ] -> cmd_run ~depth:(int_of_string d) path
  | [ _; "compile"; path ] -> cmd_compile path
  | [ _; "compile"; path; "--qasm3" ] -> cmd_compile ~target:`Qasm3 path
  | [ _; "compile"; path; "--concurrent" ] -> cmd_compile ~target:`Concurrent path
  | [ _; "simulate"; path ] -> cmd_simulate path
  | [ _; "simulate"; path; "--concurrent" ] -> cmd_simulate ~concurrent:true path
  | [ _; "repl" ] ->
      print_endline "Welcome to the interactive interpreter!";
      Interpreter.interactive_prompt []
  | _ -> usage ()
