(* ============================================================================
   Minimal statevector simulator for the OpenQASM 2.0 subset emitted by
   Compile.compile_to_qasm (h, x, z, cx, measure, if(creg==1) x/z).

   Measurements BRANCH the execution instead of sampling: amplitudes are left
   unnormalized after projection, so a branch's squared norm is its
   probability, and branches with (near) zero probability are dropped.
   Qubit k is bit k of the state index (little-endian).

   Purpose: lock the PHYSICAL correctness of compiled protocols in dune test
   (e.g. test_teleport_simulates checks that the compiled teleportation
   circuit reproduces the message state on Bob's qubit in every branch).
   ========================================================================== *)

type instr =
  | H of int
  | X of int
  | Z of int
  | CX of int * int
  | Measure of int * string (* qubit, creg name *)
  | IfX of string * int     (* apply X if creg = 1 *)
  | IfZ of string * int

type branch = {
  amp : Complex.t array;          (* unnormalized statevector *)
  cregs : (string * int) list;    (* measured classical bits *)
}

let strip_comment (l : string) : string =
  let n = String.length l in
  let rec go i =
    if i + 1 >= n then l
    else if l.[i] = '/' && l.[i + 1] = '/' then String.sub l 0 i
    else go (i + 1)
  in
  go 0

(* -> (number of qubits, instruction list); fails on an unrecognized line so
   emitter drift is caught by the tests rather than silently ignored *)
let parse (qasm : string) : int * instr list =
  let nq = ref 0 in
  let instrs = ref [] in
  let scan l fmt f = try Some (Scanf.sscanf l fmt f) with _ -> None in
  String.split_on_char '\n' qasm
  |> List.iter (fun line ->
         let l = String.trim (strip_comment line) in
         if
           l = ""
           || String.starts_with ~prefix:"OPENQASM" l
           || String.starts_with ~prefix:"include" l
           || String.starts_with ~prefix:"creg" l
         then ()
         else if
           match scan l "qreg q[%d];" (fun n -> nq := n) with
           | Some () -> true
           | None -> false
         then ()
         else
           let i =
             match scan l "h q[%d];" (fun a -> H a) with
             | Some x -> Some x
             | None -> (
                 match scan l "x q[%d];" (fun a -> X a) with
                 | Some x -> Some x
                 | None -> (
                     match scan l "z q[%d];" (fun a -> Z a) with
                     | Some x -> Some x
                     | None -> (
                         match scan l "cx q[%d], q[%d];" (fun a b -> CX (a, b)) with
                         | Some x -> Some x
                         | None -> (
                             match
                               scan l "measure q[%d] -> %[a-zA-Z0-9_][0];" (fun a r ->
                                   Measure (a, r))
                             with
                             | Some x -> Some x
                             | None -> (
                                 match
                                   scan l "if(%[a-zA-Z0-9_]==1) x q[%d];" (fun r a ->
                                       IfX (r, a))
                                 with
                                 | Some x -> Some x
                                 | None ->
                                     scan l "if(%[a-zA-Z0-9_]==1) z q[%d];" (fun r a ->
                                         IfZ (r, a)))))))
           in
           match i with
           | Some x -> instrs := x :: !instrs
           | None -> failwith ("Sim: cannot parse line: " ^ l));
  (!nq, List.rev !instrs)

(* apply a 1-qubit gate given as (amp0, amp1) -> (amp0', amp1') on qubit k *)
let apply1 f k br =
  let bit = 1 lsl k in
  let a = Array.copy br.amp in
  Array.iteri
    (fun idx _ ->
      if idx land bit = 0 then begin
        let y0, y1 = f br.amp.(idx) br.amp.(idx lor bit) in
        a.(idx) <- y0;
        a.(idx lor bit) <- y1
      end)
    br.amp;
  { br with amp = a }

let scale s c = Complex.mul { Complex.re = s; im = 0.0 } c

let h_pair x0 x1 =
  let s = 1.0 /. sqrt 2.0 in
  (Complex.add (scale s x0) (scale s x1), Complex.sub (scale s x0) (scale s x1))

let x_pair x0 x1 = (x1, x0)
let z_pair x0 x1 = (x0, Complex.neg x1)

let apply_cx c t br =
  let bc = 1 lsl c and bt = 1 lsl t in
  let a = Array.copy br.amp in
  Array.iteri
    (fun idx _ ->
      if idx land bc <> 0 && idx land bt = 0 then begin
        a.(idx) <- br.amp.(idx lor bt);
        a.(idx lor bt) <- br.amp.(idx)
      end)
    br.amp;
  { br with amp = a }

let norm2 amp = Array.fold_left (fun acc c -> acc +. Complex.norm2 c) 0.0 amp

let measure k reg br =
  let bit = 1 lsl k in
  let project v =
    Array.mapi (fun idx a -> if idx land bit <> 0 = v then a else Complex.zero) br.amp
  in
  [ false; true ]
  |> List.map (fun v ->
         { amp = project v; cregs = (reg, if v then 1 else 0) :: br.cregs })
  |> List.filter (fun b -> norm2 b.amp > 1e-12)

let creg_value br r = match List.assoc_opt r br.cregs with Some v -> v | None -> 0

(* Run the program. [init0] sets qubit 0 to alpha|0> + beta|1> (all other
   qubits start at |0>); by default everything starts at |0...0>. *)
let run ?init0 (qasm : string) : branch list =
  let nq, instrs = parse qasm in
  let dim = 1 lsl max 1 nq in
  let amp = Array.make dim Complex.zero in
  (match init0 with
  | Some (a, b) ->
      amp.(0) <- a;
      amp.(1) <- b
  | None -> amp.(0) <- Complex.one);
  let step branches ins =
    match ins with
    | H k -> List.map (apply1 h_pair k) branches
    | X k -> List.map (apply1 x_pair k) branches
    | Z k -> List.map (apply1 z_pair k) branches
    | CX (c, t) -> List.map (apply_cx c t) branches
    | Measure (k, r) -> List.concat_map (measure k r) branches
    | IfX (r, k) ->
        List.map (fun br -> if creg_value br r = 1 then apply1 x_pair k br else br) branches
    | IfZ (r, k) ->
        List.map (fun br -> if creg_value br r = 1 then apply1 z_pair k br else br) branches
  in
  List.fold_left step [ { amp; cregs = [] } ] instrs
