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

let cmd_compile ?(target = `Flat) ?(width = 1) path =
  let membranes = parse_file path in
  let _ = check_or_exit membranes in
  let circuit = Compile.lower ~width membranes in
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

let cmd_simulate ?(concurrent = false) ?(width = 1) path =
  let membranes = parse_file path in
  let _ = check_or_exit membranes in
  let branches =
    if concurrent then Sim.run_ir (Compile.lower ~width membranes)
    else Sim.run (Compile.compile_to_qasm ~width membranes)
  in
  Printf.printf "%d branch(es); qubits start at |0...0>%s:\n" (List.length branches)
    (if concurrent then " (concurrent runtime)" else "");
  List.iter print_branch branches

(* interactive REPL over the abstract machine (semantics.ml) *)
let machine_repl () =
  print_endline "QAM machine REPL — configurations step through the paper's LTS.";
  let rec loop (cfg : Ast.membrane list) =
    print_endline "\nOptions:";
    print_endline "1. Enter a new configuration";
    print_endline "2. Show current configuration";
    print_endline "3. List possible steps";
    print_endline "4. Take step N";
    print_endline "5. Run to terminal configurations";
    print_endline "6. Typecheck";
    print_endline "7. Exit";
    print_string "Choose an option: ";
    match read_line () with
    | "1" -> (
        print_string "Configuration: ";
        let input = read_line () in
        match Interpreter.parse_membranes_from_string input with
        | Ok cfg' -> loop cfg'
        | Error e ->
            Interpreter.print_parse_error e;
            loop cfg)
    | "2" ->
        print_endline (Semantics.string_of_config cfg);
        loop cfg
    | "3" ->
        let succs = Semantics.step cfg in
        if succs = [] then print_endline "No steps: terminal configuration."
        else
          List.iteri
            (fun i (l, cfg') ->
              Printf.printf "%d. --%s--> %s\n" (i + 1)
                (Semantics.string_of_label l)
                (Semantics.string_of_config cfg'))
            succs;
        loop cfg
    | "4" -> (
        print_string "Step number: ";
        let succs = Semantics.step cfg in
        match int_of_string_opt (read_line ()) with
        | Some n when n >= 1 && n <= List.length succs ->
            let l, cfg' = List.nth succs (n - 1) in
            Printf.printf "--%s--> %s\n" (Semantics.string_of_label l)
              (Semantics.string_of_config cfg');
            loop cfg'
        | _ ->
            print_endline "No such step.";
            loop cfg)
    | "5" ->
        List.iter
          (fun (t, tr) ->
            Printf.printf "terminal: %s\n  trace : [%s]\n"
              (Semantics.string_of_config t)
              (String.concat "; " tr))
          (Semantics.reachable_terminals ~depth:32 cfg);
        loop cfg
    | "6" ->
        (match Typecheck.check cfg with
        | Ok _ -> print_endline "Well-typed."
        | Error errs ->
            List.iter
              (fun e -> print_endline ("Type error: " ^ Typecheck.string_of_error e))
              errs);
        loop cfg
    | "7" -> print_endline "Exiting."
    | _ ->
        print_endline "Invalid option.";
        loop cfg
  in
  loop []

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
  print_endline "  qam run FILE.qam        execute the abstract machine (LTS) to its terminals";
  print_endline "  qam repl                interactive machine REPL (step through the LTS)";
  print_endline "Flags: --qasm3 --concurrent --width N (compile/simulate), --depth N (run)"

let () =
  match Array.to_list Sys.argv with
  | _ :: cmd :: rest -> (
      let path = ref None in
      let qasm3 = ref false and concurrent = ref false in
      let width = ref 1 and depth = ref 32 in
      let bad = ref false in
      let rec flags = function
        | [] -> ()
        | "--qasm3" :: tl -> qasm3 := true; flags tl
        | "--concurrent" :: tl -> concurrent := true; flags tl
        | "--width" :: n :: tl -> width := int_of_string n; flags tl
        | "--depth" :: n :: tl -> depth := int_of_string n; flags tl
        | a :: tl when String.length a > 0 && a.[0] <> '-' && !path = None ->
            path := Some a; flags tl
        | _ :: tl -> bad := true; flags tl
      in
      flags rest;
      let target = if !qasm3 then `Qasm3 else if !concurrent then `Concurrent else `Flat in
      match (cmd, !path, !bad) with
      | "check", Some p, false -> cmd_check p
      | "run", Some p, false -> cmd_run ~depth:!depth p
      | "compile", Some p, false -> cmd_compile ~target ~width:!width p
      | "simulate", Some p, false -> cmd_simulate ~concurrent:!concurrent ~width:!width p
      | "repl", None, false -> machine_repl ()
      | _ -> usage ())
  | _ -> usage ()
