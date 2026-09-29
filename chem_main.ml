open Chemistry

let read_file (path : string) : string =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let print_result (p : problem) (r : vqe_result) =
  let delta = r.final_energy -. p.reference_energy in
  Printf.printf "== %s ==\n" p.name;
  Printf.printf "final energy: %.8f\n" r.final_energy;
  Printf.printf "reference    : %.8f\n" p.reference_energy;
  Printf.printf "delta        : %.8f\n" delta;
  Printf.printf "iterations   : %d\n" (List.length r.history);
  Printf.printf "final theta  : %.8f\n" r.final_theta;
  Printf.printf "--- OpenQASM ---\n%s\n" r.qasm

let run_benchmarks () =
  let rows = benchmark_suite () in
  print_endline "Benchmark summary:";
  List.iter
    (fun (p, r) ->
      let delta = r.final_energy -. p.reference_energy in
      Printf.printf
        "%s | E=%.6f | ref=%.6f | delta=%.6f | steps=%d\n"
        p.name r.final_energy p.reference_energy delta
        (List.length r.history))
    rows

let () =
  let argv = Array.to_list Sys.argv in
  match argv with
  | [ _; "--benchmark" ] -> run_benchmarks ()
  | [ _; "--dsl"; path ] -> (
      let contents = read_file path in
      match parse_problem contents with
      | Ok p ->
          let r = run_vqe p in
          print_result p r
      | Error msg ->
          prerr_endline ("DSL parse error: " ^ msg);
          exit 1)
  | _ ->
      print_endline "Usage:";
      print_endline "  dune exec -- chem_main -- --benchmark";
      print_endline "  dune exec -- chem_main -- --dsl examples/chem/h2.qamchem"
