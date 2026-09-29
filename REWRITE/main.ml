let read_file (path : string) : string =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let compile_file (path : string) =
  match Interpreter.parse_membranes_from_string (read_file path) with
  | Ok membranes -> print_string (Compile.compile_to_qasm membranes)
  | Error e ->
      Interpreter.print_parse_error e;
      exit 1

let () =
  match Array.to_list Sys.argv with
  | [ _; "--compile"; path ] -> compile_file path
  | [ _ ] ->
      print_endline "Welcome to the interactive interpreter!";
      Interpreter.interactive_prompt []
  | _ ->
      print_endline "Usage:";
      print_endline "  dune exec -- main                                # interactive REPL";
      print_endline "  dune exec -- main -- --compile FILE.qam          # compile to OpenQASM"
