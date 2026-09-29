open Ast

(* ============================================================================
   Path B / target (a): QAM configuration -> flat OpenQASM 2.0

   Syntax-directed backend following Li et al., "The Quantum Abstract Machine"
   (arXiv:2402.13469v1), Figure 13 / Appendix E. Codegen recurses on the
   *structure* of processes and membranes, independent of the reduction relation.

   Layout (the paper's Sigma, with N = 1): every resource molecule occupies one
   qubit; membrane i's qubit region follows membrane i-1's. Blank resources `o`
   (the paper's ◦) are claimed, in order, by channel creation — Cohere requires
   one blank per party — and named resources are addressable as encode messages.
   The paper's single global state phi is one qreg; classical residues live in
   1-bit cregs.

   Encode/decode are lowered to the paper's CONCRETE teleportation circuit
   (Figure 10) rather than Figure 13's letter: quantum encode is CNOT(msg ->
   channel) then H(msg), and decode measures both the channel qubit (X bit) and
   the encoded payload qubit (Z bit). Figure 13's C-EncodeQ as written puts H on
   the channel qubit and measures only the channel, which does not reproduce
   teleportation on a statevector; the Figure 10 lowering is verified by
   simulation (see sim.ml and test_teleport_simulates).

   MVP simplifications (see REWRITE/README.md "Path B"):
   - N = 1 qubit per quantum message; one encode per channel.
   - send/wait synchronizers are linearized into gate ordering (no scheduler),
     so a classical channel's sender membrane must precede its receiver, and
     each classical channel carries one residue.
   - an encode message that resolves to nothing becomes a 1-bit classical INPUT
     creg named after it (superdense coding's classical message).
   - only top-level MoleculeMembranes contribute gates.
   ========================================================================== *)

let suffix_after (s : string) (sep : string) : string =
  let sl = String.length s and nl = String.length sep in
  let rec go i =
    if i + nl > sl then s
    else if String.sub s i nl = sep then String.sub s (i + nl) (sl - i - nl)
    else go (i + 1)
  in
  go 0

let membrane_processes = function
  | MoleculeMembrane ms ->
      List.filter_map (function ProcessMolecule p -> Some p | _ -> None) ms
  | _ -> []

let membrane_resources = function
  | MoleculeMembrane ms ->
      List.filter_map (function ResourceMolecule r -> Some r | _ -> None) ms
  | _ -> []

(* Channel names a process creates via `nu c.` (both parties run it). *)
let rec created_channels = function
  | NullProcess -> []
  | ActionProcess (NewChannel c, k) -> c :: created_channels k
  | ActionProcess (_, k) -> created_channels k
  | Choice (a, b) -> created_channels a @ created_channels b
  | Replication r -> created_channels r

(* A quantum channel's two parties. First membrane creating it is the left end
   (drives the Bell pair, C-CohereL); the second is the passive right end. *)
type party = {
  lmem : int;
  lqubit : int option; (* None: that membrane had no free blank `o` *)
  mutable rmem : int option;
  mutable rqubit : int option;
}

(* -> (total qubits, (membrane, resource name) -> qubit, channel -> party) *)
let build_layout (membranes : membrane list) =
  let next = ref 0 in
  let named : (int * string, int) Hashtbl.t = Hashtbl.create 16 in
  let blanks : (int, int Queue.t) Hashtbl.t = Hashtbl.create 8 in
  List.iteri
    (fun i m ->
      let q = Queue.create () in
      Hashtbl.replace blanks i q;
      List.iter
        (fun r ->
          let idx = !next in
          incr next;
          match r with
          | NullResource -> Queue.add idx q
          | SimpleResource name -> Hashtbl.replace named (i, name) idx
          | CombinedResource _ | MeetOperation _ ->
              () (* occupies a qubit, not addressable by name *))
        (membrane_resources m))
    membranes;
  let parties : (string, party) Hashtbl.t = Hashtbl.create 16 in
  let take_blank i =
    match Hashtbl.find_opt blanks i with
    | Some q when not (Queue.is_empty q) -> Some (Queue.pop q)
    | _ -> None
  in
  List.iteri
    (fun i m ->
      List.iter
        (fun p ->
          List.iter
            (fun c ->
              match Hashtbl.find_opt parties c with
              | None ->
                  Hashtbl.replace parties c
                    { lmem = i; lqubit = take_blank i; rmem = None; rqubit = None }
              | Some pt when pt.rmem = None && pt.lmem <> i ->
                  pt.rmem <- Some i;
                  pt.rqubit <- take_blank i
              | Some _ -> ())
            (created_channels p))
        (membrane_processes m))
    membranes;
  (!next, named, parties)

(* qubit of channel c's party inside membrane g *)
let channel_qubit parties c g =
  match Hashtbl.find_opt parties c with
  | Some pt when pt.lmem = g -> pt.lqubit
  | Some pt when pt.rmem = Some g -> pt.rqubit
  | _ -> None

let compile_to_qasm (membranes : membrane list) : string =
  let nqubits, named, parties = build_layout membranes in
  let body = Buffer.create 512 in
  (* creg declarations in first-use order: (name, trailing comment) *)
  let cregs : (string * string) list ref = ref [] in
  let declare_creg ?(comment = "") name =
    if not (List.mem_assoc name !cregs) then cregs := !cregs @ [ (name, comment) ]
  in
  let fresh = ref 0 in
  let fresh_creg () =
    let r = Printf.sprintf "m%d" !fresh in
    incr fresh;
    declare_creg r;
    r
  in
  (* channel -> qubit of the quantum message encoded onto it (Fig. 10 payload) *)
  let payload : (string, int) Hashtbl.t = Hashtbl.create 8 in
  (* classical channel -> residue cregs (X bit, optional Z bit) last sent on it *)
  let classical : (string, string * string option) Hashtbl.t = Hashtbl.create 8 in
  (* env binds receive/decode-bound names in the current process:
     `Chan c        — the name stands for (a party of) quantum channel c
     `Res (xb, zb)  — a classical residue: X-correction bit, optional Z bit *)
  let resolve_chan env name =
    match List.assoc_opt name env with Some (`Chan c) -> c | _ -> name
  in
  let rec emit g (env : (string * [ `Chan of string | `Res of string * string option ]) list) p =
    match p with
    | NullProcess -> ()
    | ActionProcess (NewChannel c, k) ->
        (match Hashtbl.find_opt parties c with
        | Some pt when pt.lmem = g -> (
            match (pt.lqubit, pt.rqubit) with
            | Some i, Some j ->
                (* C-CohereL: Bell pair across the two parties' blanks *)
                Printf.bprintf body "h q[%d];\n" i;
                Printf.bprintf body "cx q[%d], q[%d];\n" i j
            | _ ->
                Printf.bprintf body
                  "// ERROR: channel %s needs a blank 'o' in each of its two membranes (Cohere)\n"
                  c)
        | Some _ -> () (* C-CohereR: passive right end *)
        | None -> ());
        emit g env k
    | ActionProcess (LeftCombine s, k) ->
        let chan = resolve_chan env (Interpreter.action_channel (LeftCombine s)) in
        let msg = suffix_after s "<-" in
        (match channel_qubit parties chan g with
        | None ->
            Printf.bprintf body "// ERROR: encode on %s: no channel party in this membrane\n" chan
        | Some i -> (
            (* resolve the message: classical residue, named quantum resource,
               a channel party used as quantum message (entanglement swap), or
               an otherwise-unknown name, treated as a classical input bit *)
            let msg_val =
              match List.assoc_opt msg env with
              | Some (`Res (xb, zb)) -> `Classical (xb, zb)
              | Some (`Chan c') -> (
                  match channel_qubit parties c' g with
                  | Some mq -> `Quantum mq
                  | None -> `Unresolved)
              | None -> (
                  match Hashtbl.find_opt named (g, msg) with
                  | Some mq -> `Quantum mq
                  | None -> (
                      match channel_qubit parties msg g with
                      | Some mq -> `Quantum mq
                      | None -> `Input))
            in
            match msg_val with
            | `Quantum mq ->
                (* Encode block, Fig. 10: CNOT message -> channel, then H message.
                   Remember the payload so decode measures it for the Z bit. *)
                Printf.bprintf body "cx q[%d], q[%d];\n" mq i;
                Printf.bprintf body "h q[%d];\n" mq;
                Hashtbl.replace payload chan mq
            | `Classical (xb, zb) ->
                (* Recover block, Fig. 10: X by the channel bit, Z by the payload bit *)
                let zbit = match zb with Some z -> z | None -> xb in
                Printf.bprintf body "if(%s==1) x q[%d];\n" xb i;
                Printf.bprintf body "if(%s==1) z q[%d];\n" zbit i
            | `Input ->
                (* classical message literal (superdense coding): a 1-bit input *)
                declare_creg ~comment:" // classical input bit" msg;
                Printf.bprintf body "if(%s==1) x q[%d];\n" msg i;
                Printf.bprintf body "if(%s==1) z q[%d];\n" msg i
            | `Unresolved ->
                Printf.bprintf body "// ERROR: encode on %s: unknown message %s\n" chan msg));
        emit g env k
    | ActionProcess (RightCombine s, k) ->
        (* Decode block, Fig. 10: measure the channel qubit (X bit) and, if a
           message was encoded, the payload qubit (Z bit); bind the residue *)
        let chan = resolve_chan env (Interpreter.action_channel (RightCombine s)) in
        let binder = suffix_after s "->" in
        (match channel_qubit parties chan g with
        | Some i ->
            let xb = fresh_creg () in
            Printf.bprintf body "measure q[%d] -> %s[0]; // decode channel %s\n" i xb chan;
            let zb =
              match Hashtbl.find_opt payload chan with
              | Some mq ->
                  let z = fresh_creg () in
                  Printf.bprintf body "measure q[%d] -> %s[0]; // decode %s payload\n" mq z chan;
                  Some z
              | None -> None
            in
            emit g ((binder, `Res (xb, zb)) :: env) k
        | None ->
            Printf.bprintf body "// ERROR: decode on %s: no channel party in this membrane\n" chan;
            emit g env k)
    | ActionProcess (Send { chan; arg }, k) ->
        (* Com: classical send; record which residue travels on this channel *)
        let chan = resolve_chan env chan in
        (match List.assoc_opt arg env with
        | Some (`Res r) -> Hashtbl.replace classical chan r
        | _ -> ());
        Printf.bprintf body "// classical send of %s on channel %s\n" arg chan;
        emit g env k
    | ActionProcess (Receive { chan; arg }, k) ->
        (* C-Rev: synchronizer. On a quantum channel the binder stands for the
           channel (projective-channel notification); on a classical channel it
           stands for the residue the sender recorded. *)
        let chan = resolve_chan env chan in
        let env' =
          if Hashtbl.mem parties chan then (
            Printf.bprintf body "// wait on quantum channel %s (decode notification)\n" chan;
            (arg, `Chan chan) :: env)
          else
            match Hashtbl.find_opt classical chan with
            | Some ((xb, _) as r) ->
                Printf.bprintf body "// receive %s := %s (classical channel %s)\n" arg xb chan;
                (arg, `Res r) :: env
            | None ->
                Printf.bprintf body "// wait on channel %s (synchronizer)\n" chan;
                env
        in
        emit g env' k
    | Choice (l, _r) -> emit g env l (* commit-left, matching the reducer (CL) *)
    | Replication r -> emit g env r (* one local copy (MT, bounded) *)
  in
  List.iteri
    (fun i m ->
      Printf.bprintf body "// --- membrane %d ---\n" i;
      List.iter (emit i []) (membrane_processes m))
    membranes;
  let header = Buffer.create 128 in
  Buffer.add_string header "OPENQASM 2.0;\ninclude \"qelib1.inc\";\n";
  Printf.bprintf header "qreg q[%d];\n" (max 1 nqubits);
  List.iter (fun (n, cmt) -> Printf.bprintf header "creg %s[1];%s\n" n cmt) !cregs;
  Buffer.contents header ^ Buffer.contents body
