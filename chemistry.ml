type pauli = I | X | Y | Z

type term = {
  coeff : float;
  paulis : (int * pauli) list;
}

type problem = {
  name : string;
  n_qubits : int;
  terms : term list;
  reference_energy : float;
}

type config = {
  steps : int;
  learning_rate : float;
  init_theta : float;
  layers : int;
}

type vqe_result = {
  final_theta : float;
  final_energy : float;
  history : (int * float) list;
  qasm : string;
}

let default_config = { steps = 120; learning_rate = 0.08; init_theta = 0.2; layers = 1 }

let strip (s : string) : string =
  String.trim s

let split_words (s : string) : string list =
  s |> String.split_on_char ' ' |> List.filter (fun w -> w <> "")

let parse_pauli (s : string) : (int * pauli) option =
  if String.length s < 2 then None
  else
    let head = s.[0] in
    let idx_str = String.sub s 1 (String.length s - 1) in
    let p =
      match head with
      | 'I' -> Some I
      | 'X' -> Some X
      | 'Y' -> Some Y
      | 'Z' -> Some Z
      | _ -> None
    in
    match p with
    | None -> None
    | Some pauli -> (
        try Some (int_of_string idx_str, pauli) with _ -> None)

let parse_term_tokens (tokens : string list) : term option =
  match tokens with
  | "term" :: coeff_s :: ops ->
      let coeff = try Some (float_of_string coeff_s) with _ -> None in
      let paulis = List.filter_map parse_pauli ops in
      if List.length paulis = List.length ops then
        Option.map (fun c -> { coeff = c; paulis }) coeff
      else None
  | _ -> None

let parse_problem (input : string) : (problem, string) result =
  let lines =
    input
    |> String.split_on_char '\n'
    |> List.map strip
    |> List.filter (fun l -> l <> "" && not (String.starts_with ~prefix:"#" l))
  in
  let rec fold lines name qubits ref_e terms =
    match lines with
    | [] -> (
        match (name, qubits, ref_e) with
        | Some n, Some q, Some r -> Ok { name = n; n_qubits = q; terms = List.rev terms; reference_energy = r }
        | _ -> Error "DSL missing one of required fields: name/qubits/ref")
    | line :: rest ->
        let toks = split_words line in
        (match toks with
        | ["name"; n] -> fold rest (Some n) qubits ref_e terms
        | ["qubits"; q] -> (
            try fold rest name (Some (int_of_string q)) ref_e terms
            with _ -> Error ("invalid qubits line: " ^ line))
        | ["ref"; r] -> (
            try fold rest name qubits (Some (float_of_string r)) terms
            with _ -> Error ("invalid ref line: " ^ line))
        | "term" :: _ -> (
            match parse_term_tokens toks with
            | Some t -> fold rest name qubits ref_e (t :: terms)
            | None -> Error ("invalid term line: " ^ line))
        | _ -> Error ("unrecognized DSL line: " ^ line))
  in
  fold lines None None None []

let expectation_single_pauli theta = function
  | I -> 1.0
  | Z -> Float.cos theta
  | X -> Float.sin theta
  | Y -> 0.0

let expectation_term theta (t : term) : float =
  t.paulis
  |> List.map (fun (_, p) -> expectation_single_pauli theta p)
  |> List.fold_left ( *. ) 1.0
  |> fun exp_val -> t.coeff *. exp_val

let energy theta (p : problem) : float =
  List.fold_left (fun acc t -> acc +. expectation_term theta t) 0.0 p.terms

let finite_diff_grad f theta =
  let eps = 1e-5 in
  (f (theta +. eps) -. f (theta -. eps)) /. (2.0 *. eps)

let export_openqasm (p : problem) ~theta ~layers : string =
  let b = Buffer.create 256 in
  Buffer.add_string b "OPENQASM 2.0;\ninclude \"qelib1.inc\";\n";
  Buffer.add_string b (Printf.sprintf "qreg q[%d];\n" p.n_qubits);
  for _ = 1 to layers do
    for i = 0 to p.n_qubits - 1 do
      Buffer.add_string b (Printf.sprintf "ry(%0.8f) q[%d];\n" theta i)
    done
  done;
  Buffer.contents b

let run_vqe ?(cfg = default_config) (p : problem) : vqe_result =
  let objective t = energy t p in
  let rec loop step theta hist =
    if step > cfg.steps then
      let e = objective theta in
      { final_theta = theta; final_energy = e; history = List.rev ((step, e) :: hist); qasm = export_openqasm p ~theta ~layers:cfg.layers }
    else
      let e = objective theta in
      let g = finite_diff_grad objective theta in
      let next_theta = theta -. (cfg.learning_rate *. g) in
      loop (step + 1) next_theta ((step, e) :: hist)
  in
  loop 0 cfg.init_theta []

let builtin_h2 =
  {
    name = "H2";
    n_qubits = 2;
    reference_energy = -1.137;
    terms =
      [
        { coeff = -1.0523732; paulis = [ (0, I) ] };
        { coeff = 0.3979374; paulis = [ (0, Z) ] };
        { coeff = -0.3979374; paulis = [ (1, Z) ] };
        { coeff = -0.0112801; paulis = [ (0, Z); (1, Z) ] };
        { coeff = 0.1809312; paulis = [ (0, X); (1, X) ] };
      ];
  }

let builtin_lih =
  {
    name = "LiH";
    n_qubits = 4;
    reference_energy = -7.882;
    terms =
      [
        { coeff = -7.4989469; paulis = [ (0, I) ] };
        { coeff = 0.171201; paulis = [ (0, Z) ] };
        { coeff = -0.222796; paulis = [ (1, Z) ] };
        { coeff = 0.120546; paulis = [ (2, Z) ] };
        { coeff = -0.100321; paulis = [ (3, Z) ] };
        { coeff = 0.06734; paulis = [ (0, X); (1, X) ] };
        { coeff = 0.05312; paulis = [ (2, X); (3, X) ] };
      ];
  }

let benchmark_suite () : (problem * vqe_result) list =
  [ builtin_h2; builtin_lih ] |> List.map (fun p -> (p, run_vqe p))
