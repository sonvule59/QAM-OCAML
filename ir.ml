(* ============================================================================
   Circuit IR (Phase 3): the typed intermediate representation between QAM
   lowering and OpenQASM emission.

   A [circuit] keeps one instruction list PER MEMBRANE, with classical
   synchronization (`send`/`wait`) as first-class instructions. The flat
   OpenQASM backend then runs passes:

     schedule      list-schedule the per-membrane programs into one linear
                   program, blocking a Wait until its channel's Send has been
                   emitted (replaces the old "sender membrane must precede the
                   receiver" restriction) — an unservable Wait is a detected
                   DEADLOCK;
     cancel_gates  peephole: adjacent self-inverse pairs (H H, X X, Z Z,
                   CX CX) cancel, to a fixpoint;
     compact       dead-qubit elimination: qubits no instruction touches are
                   removed and the register renumbered;
     emit          OpenQASM 2.0 text.

   The IR is shared ground for the future concurrent target (b), which will
   keep the per-membrane programs separate instead of scheduling them.
   ========================================================================== *)

type instr =
  | H of int
  | X of int
  | Z of int
  | CX of int * int
  | Measure of { q : int; c : string; note : string } (* note appended to line *)
  | CtrlX of { c : string; q : int } (* if(c==1) x q *)
  | CtrlZ of { c : string; q : int }
  | SendC of { chan : string; desc : string } (* classical sync, comment only *)
  | WaitC of { chan : string; desc : string }
  | Note of string (* plain comment line (errors, diagnostics) *)

type proc = { mem : int; body : instr list }

type circuit = {
  nqubits : int;
  cregs : (string * string) list; (* name, trailing comment *)
  procs : proc list;
  regions : (int * int list) list; (* membrane -> qubits it owns (its Sigma slots) *)
}

(* a creg is an INPUT parameter (superdense's classical message) if lowering
   marked it so; everything else is a measurement bit *)
let is_input_creg (_, comment) =
  let has_sub s sub =
    let sl = String.length s and bl = String.length sub in
    let rec go i = i + bl <= sl && (String.sub s i bl = sub || go (i + 1)) in
    go 0
  in
  has_sub comment "input"

let qubits_of = function
  | H q | X q | Z q -> [ q ]
  | CX (a, b) -> [ a; b ]
  | Measure { q; _ } | CtrlX { q; _ } | CtrlZ { q; _ } -> [ q ]
  | SendC _ | WaitC _ | Note _ -> []

(* ---- scheduling ----------------------------------------------------------- *)

(* Linearize the per-membrane programs. A WaitC blocks while some proc still
   holds an unemitted SendC on that channel; if every nonempty proc is blocked
   we have a synchronization deadlock: report it and flush what remains. *)
let schedule (c : circuit) : (int * instr) list =
  let senders = Hashtbl.create 8 in
  List.iter
    (fun p ->
      List.iter
        (function SendC { chan; _ } -> Hashtbl.replace senders chan () | _ -> ())
        p.body)
    c.procs;
  let bodies = Array.of_list (List.map (fun p -> ref p.body) c.procs) in
  let mems = Array.of_list (List.map (fun p -> p.mem) c.procs) in
  let sent = Hashtbl.create 8 in
  let out = ref [] in
  let emit mem i =
    (match i with SendC { chan; _ } -> Hashtbl.replace sent chan () | _ -> ());
    out := (mem, i) :: !out
  in
  let blocked = function
    | WaitC { chan; _ } -> Hashtbl.mem senders chan && not (Hashtbl.mem sent chan)
    | _ -> false
  in
  let progress = ref true in
  while !progress do
    progress := false;
    Array.iteri
      (fun k body ->
        let rec drain () =
          match !body with
          | i :: tl when not (blocked i) ->
              emit mems.(k) i;
              body := tl;
              progress := true;
              drain ()
          | _ -> ()
        in
        drain ())
      bodies
  done;
  (* anything left is deadlocked: every head is a blocked Wait *)
  let leftovers = Array.exists (fun b -> !b <> []) bodies in
  if leftovers then begin
    let chans =
      Array.to_list bodies
      |> List.concat_map (fun b ->
             match !b with WaitC { chan; _ } :: _ -> [ chan ] | _ -> [])
      |> List.sort_uniq compare
    in
    out :=
      ( -1,
        Note
          (Printf.sprintf
             "ERROR: classical synchronization deadlock on channel(s): %s"
             (String.concat ", " chans)) )
      :: !out;
    Array.iteri
      (fun k body ->
        List.iter (fun i -> out := (mems.(k), i) :: !out) !body;
        body := [])
      bodies
  end;
  List.rev !out

(* ---- peephole: adjacent self-inverse gate pairs cancel -------------------- *)

let cancel_gates (instrs : (int * instr) list) : (int * instr) list =
  let rec pass = function
    | (_, H a) :: (_, H b) :: tl when a = b -> pass tl
    | (_, X a) :: (_, X b) :: tl when a = b -> pass tl
    | (_, Z a) :: (_, Z b) :: tl when a = b -> pass tl
    | (_, CX (a, b)) :: (_, CX (a', b')) :: tl when a = a' && b = b' -> pass tl
    | i :: tl -> i :: pass tl
    | [] -> []
  in
  let rec fix xs =
    let xs' = pass xs in
    if List.length xs' = List.length xs then xs else fix xs'
  in
  fix instrs

(* ---- dead-qubit elimination ----------------------------------------------- *)

let compact (nqubits : int) (instrs : (int * instr) list) :
    int * (int * instr) list =
  let used = Array.make (max 1 nqubits) false in
  List.iter
    (fun (_, i) -> List.iter (fun q -> if q < Array.length used then used.(q) <- true) (qubits_of i))
    instrs;
  let remap = Hashtbl.create 8 in
  let next = ref 0 in
  Array.iteri
    (fun q u ->
      if u then begin
        Hashtbl.replace remap q !next;
        incr next
      end)
    used;
  let r q = match Hashtbl.find_opt remap q with Some q' -> q' | None -> q in
  let rewrite = function
    | H q -> H (r q)
    | X q -> X (r q)
    | Z q -> Z (r q)
    | CX (a, b) -> CX (r a, r b)
    | Measure m -> Measure { m with q = r m.q }
    | CtrlX m -> CtrlX { m with q = r m.q }
    | CtrlZ m -> CtrlZ { m with q = r m.q }
    | (SendC _ | WaitC _ | Note _) as i -> i
  in
  (max 1 !next, List.map (fun (m, i) -> (m, rewrite i)) instrs)

(* ---- emission -------------------------------------------------------------- *)

(* instructions whose qubits are not all inside the owning membrane's region.
   Per the paper, channel creation (Cohere's Bell pair) is the one joint,
   collaborative operation; everything else must stay local. *)
let cross_region (c : circuit) : (int * instr) list =
  List.concat_map
    (fun p ->
      let region = try List.assoc p.mem c.regions with Not_found -> [] in
      List.filter_map
        (fun i ->
          let qs = qubits_of i in
          if qs <> [] && List.exists (fun q -> not (List.mem q region)) qs then
            Some (p.mem, i)
          else None)
        p.body)
    c.procs

let to_qasm (c : circuit) : string =
  let scheduled = schedule c in
  let scheduled = cancel_gates scheduled in
  let nqubits, scheduled = compact c.nqubits scheduled in
  let b = Buffer.create 512 in
  Buffer.add_string b "OPENQASM 2.0;\ninclude \"qelib1.inc\";\n";
  Printf.bprintf b "qreg q[%d];\n" nqubits;
  List.iter (fun (n, cmt) -> Printf.bprintf b "creg %s[1];%s\n" n cmt) c.cregs;
  let current_mem = ref (-2) in
  List.iter
    (fun (mem, i) ->
      if mem >= 0 && mem <> !current_mem then begin
        current_mem := mem;
        Printf.bprintf b "// --- membrane %d ---\n" mem
      end;
      match i with
      | H q -> Printf.bprintf b "h q[%d];\n" q
      | X q -> Printf.bprintf b "x q[%d];\n" q
      | Z q -> Printf.bprintf b "z q[%d];\n" q
      | CX (a, b') -> Printf.bprintf b "cx q[%d], q[%d];\n" a b'
      | Measure { q; c; note } -> Printf.bprintf b "measure q[%d] -> %s[0];%s\n" q c note
      | CtrlX { c; q } -> Printf.bprintf b "if(%s==1) x q[%d];\n" c q
      | CtrlZ { c; q } -> Printf.bprintf b "if(%s==1) z q[%d];\n" c q
      | SendC { desc; _ } | WaitC { desc; _ } -> Printf.bprintf b "// %s\n" desc
      | Note s -> Printf.bprintf b "// %s\n" s)
    scheduled;
  Buffer.contents b

(* ---- backend: OpenQASM 3.0 ------------------------------------------------
   Same schedule/cancel/compact passes as the flat QASM 2 target, but with
   real classical types: measurement results are `bit`s, classical message
   inputs (superdense coding) are `input bit` runtime parameters, and
   corrections use genuine if-statements. *)

let to_qasm3 (c : circuit) : string =
  let scheduled = schedule c in
  let scheduled = cancel_gates scheduled in
  let nqubits, scheduled = compact c.nqubits scheduled in
  let b = Buffer.create 512 in
  Buffer.add_string b "OPENQASM 3.0;\ninclude \"stdgates.inc\";\n";
  List.iter
    (fun ((n, _) as creg) ->
      if is_input_creg creg then Printf.bprintf b "input bit %s;\n" n)
    c.cregs;
  Printf.bprintf b "qubit[%d] q;\n" nqubits;
  List.iter
    (fun ((n, _) as creg) ->
      if not (is_input_creg creg) then Printf.bprintf b "bit %s;\n" n)
    c.cregs;
  let current_mem = ref (-2) in
  List.iter
    (fun (mem, i) ->
      if mem >= 0 && mem <> !current_mem then begin
        current_mem := mem;
        Printf.bprintf b "// --- membrane %d ---\n" mem
      end;
      match i with
      | H q -> Printf.bprintf b "h q[%d];\n" q
      | X q -> Printf.bprintf b "x q[%d];\n" q
      | Z q -> Printf.bprintf b "z q[%d];\n" q
      | CX (a, b') -> Printf.bprintf b "cx q[%d], q[%d];\n" a b'
      | Measure { q; c; note } -> Printf.bprintf b "%s = measure q[%d];%s\n" c q note
      | CtrlX { c; q } -> Printf.bprintf b "if (%s == 1) { x q[%d]; }\n" c q
      | CtrlZ { c; q } -> Printf.bprintf b "if (%s == 1) { z q[%d]; }\n" c q
      | SendC { desc; _ } | WaitC { desc; _ } -> Printf.bprintf b "// %s\n" desc
      | Note s -> Printf.bprintf b "// %s\n" s)
    scheduled;
  Buffer.contents b

(* ---- backend: target (b), the concurrent circuit --------------------------
   The paper's Concurrent SQIR shape: one sequential process per membrane,
   pinned to its qubit region, over a shared quantum state, synchronizing via
   explicit send/wait. NO scheduling pass runs — the processes stay separate,
   preserving the QAM's locality (apart from Cohere's collaborative Bell
   pair, flagged by [cross_region]). Executable via Sim.run_ir. *)

let to_concurrent (c : circuit) : string =
  let b = Buffer.create 512 in
  Buffer.add_string b
    "// QAM concurrent circuit (target b): one process per membrane over a\n\
     // shared quantum state; classical synchronization via send/wait.\n";
  List.iter
    (fun ((n, _) as creg) ->
      if is_input_creg creg then Printf.bprintf b "input bit %s;\n" n)
    c.cregs;
  Printf.bprintf b "shared qubit[%d] q;\n" (max 1 c.nqubits);
  List.iter
    (fun ((n, _) as creg) ->
      if not (is_input_creg creg) then Printf.bprintf b "bit %s;\n" n)
    c.cregs;
  List.iter
    (fun p ->
      let region = try List.assoc p.mem c.regions with Not_found -> [] in
      Printf.bprintf b "\nprocess %d owns q[%s] {\n" p.mem
        (String.concat "," (List.map string_of_int region));
      let body =
        List.map snd (cancel_gates (List.map (fun i -> (p.mem, i)) p.body))
      in
      List.iter
        (fun i ->
          match i with
          | H q -> Printf.bprintf b "  h q[%d];\n" q
          | X q -> Printf.bprintf b "  x q[%d];\n" q
          | Z q -> Printf.bprintf b "  z q[%d];\n" q
          | CX (a, b') -> Printf.bprintf b "  cx q[%d], q[%d];\n" a b'
          | Measure { q; c; note } -> Printf.bprintf b "  %s = measure q[%d];%s\n" c q note
          | CtrlX { c; q } -> Printf.bprintf b "  if (%s == 1) { x q[%d]; }\n" c q
          | CtrlZ { c; q } -> Printf.bprintf b "  if (%s == 1) { z q[%d]; }\n" c q
          | SendC { chan; desc } -> Printf.bprintf b "  send %s;  // %s\n" chan desc
          | WaitC { chan; desc } -> Printf.bprintf b "  wait %s;  // %s\n" chan desc
          | Note s -> Printf.bprintf b "  // %s\n" s)
        body;
      Buffer.add_string b "}\n")
    c.procs;
  Buffer.contents b
