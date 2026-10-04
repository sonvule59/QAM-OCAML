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
  | NullProcess | ProcessRef _ -> []
  | ActionProcess ({ act = NewChannel c; _ }, k) -> c :: created_channels k
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
          | ChannelParty _ | CombinedResource _ | MeetOperation _ ->
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

(* ============================================================================
   Lowering (Phase 3): QAM configuration -> Ir.circuit.

   Each membrane lowers to its own instruction list with classical send/wait
   as first-class IR; Ir.to_qasm then schedules, optimizes and emits.

   Classical dataflow is linked by FIXPOINT: lowering rounds repeat while the
   classical-channel table still grows, so a receiver membrane may precede its
   sender in the configuration (the old "sender first" restriction is gone).
   Unservable waits are reported as deadlocks by the scheduler.
   ========================================================================== *)

(* lower a configuration to the circuit IR (shared by every backend) *)
let lower (membranes : membrane list) : Ir.circuit =
  let nqubits, named, parties = build_layout membranes in
  (* each membrane's Sigma region: one slot per resource molecule, sequential *)
  let regions =
    let next = ref 0 in
    List.mapi
      (fun i m ->
        let n = List.length (membrane_resources m) in
        let r = List.init n (fun k -> !next + k) in
        next := !next + n;
        (i, r))
      membranes
  in
  (* classical channel -> residue cregs (X bit, optional Z bit); persists
     across lowering rounds so receiver-before-sender resolves next round *)
  let classical : (string, string * string option) Hashtbl.t = Hashtbl.create 8 in
  let resolve_chan env name =
    match List.assoc_opt name env with Some (`Chan c) -> c | _ -> name
  in
  let lower_round () =
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
    (* channel -> qubit of the quantum message encoded onto it (payload) *)
    let payload : (string, int) Hashtbl.t = Hashtbl.create 8 in
    let lower_membrane g m =
      let out = ref [] in
      let push i = out := i :: !out in
      let rec emit (env : (string * [ `Chan of string | `Res of string * string option ]) list) p =
        match p with
        | NullProcess -> ()
        | ActionProcess ({ act = NewChannel c; _ }, k) ->
            (match Hashtbl.find_opt parties c with
            | Some pt when pt.lmem = g -> (
                match (pt.lqubit, pt.rqubit) with
                | Some i, Some j ->
                    push (Ir.H i);
                    push (Ir.CX (i, j))
                | _ ->
                    push
                      (Ir.Note
                         (Printf.sprintf
                            "ERROR: channel %s needs a blank 'o' in each of its two membranes (Cohere)"
                            c)))
            | Some _ -> () (* passive right end *)
            | None -> ());
            emit env k
        | ActionProcess ({ act = Encode { chan; msg = msg_ast }; _ }, k) ->
            let chan = resolve_chan env chan in
            (match channel_qubit parties chan g with
            | None ->
                push
                  (Ir.Note
                     (Printf.sprintf "ERROR: encode on %s: no channel party in this membrane" chan))
            | Some i -> (
                let msg = match msg_ast with Named n -> n | m -> Interpreter.string_of_msg m in
                let msg_val =
                  match msg_ast with
                  | Blank | Channeled _ | Projected _ | Residue _ | Meet _ -> `Unresolved
                  | Named _ -> (
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
                              | None -> `Input)))
                in
                match msg_val with
                | `Quantum mq ->
                    (* Encode block, Fig. 10: CNOT message -> channel, H message *)
                    push (Ir.CX (mq, i));
                    push (Ir.H mq);
                    Hashtbl.replace payload chan mq
                | `Classical (xb, zb) ->
                    let zbit = match zb with Some z -> z | None -> xb in
                    push (Ir.CtrlX { c = xb; q = i });
                    push (Ir.CtrlZ { c = zbit; q = i })
                | `Input ->
                    declare_creg ~comment:" // classical input bit" msg;
                    push (Ir.CtrlX { c = msg; q = i });
                    push (Ir.CtrlZ { c = msg; q = i })
                | `Unresolved ->
                    push
                      (Ir.Note
                         (Printf.sprintf "ERROR: encode on %s: unknown message %s" chan msg))));
            emit env k
        | ActionProcess ({ act = Decode { chan; binder }; _ }, k) -> (
            let chan = resolve_chan env chan in
            match channel_qubit parties chan g with
            | Some i ->
                let xb = fresh_creg () in
                push
                  (Ir.Measure
                     { q = i; c = xb; note = Printf.sprintf " // decode channel %s" chan });
                let zb =
                  match Hashtbl.find_opt payload chan with
                  | Some mq ->
                      let z = fresh_creg () in
                      push
                        (Ir.Measure
                           { q = mq; c = z; note = Printf.sprintf " // decode %s payload" chan });
                      Some z
                  | None -> None
                in
                push
                  (Ir.SendC
                     { chan; desc = Printf.sprintf "decode notification on channel %s" chan });
                emit ((binder, `Res (xb, zb)) :: env) k
            | None ->
                push
                  (Ir.Note
                     (Printf.sprintf "ERROR: decode on %s: no channel party in this membrane" chan));
                emit env k)
        | ActionProcess ({ act = Send { chan; arg }; _ }, k) ->
            let chan = resolve_chan env chan in
            (match arg with
            | Named n -> (
                match List.assoc_opt n env with
                | Some (`Res r) -> Hashtbl.replace classical chan r
                | _ -> ())
            | _ -> ());
            push
              (Ir.SendC
                 {
                   chan;
                   desc =
                     Printf.sprintf "classical send of %s on channel %s"
                       (Interpreter.string_of_msg arg) chan;
                 });
            emit env k
        | ActionProcess ({ act = Receive { chan; arg }; _ }, k) ->
            let chan = resolve_chan env chan in
            if Hashtbl.mem parties chan then begin
              push
                (Ir.WaitC
                   {
                     chan;
                     desc =
                       Printf.sprintf "wait on quantum channel %s (decode notification)" chan;
                   });
              emit ((arg, `Chan chan) :: env) k
            end
            else begin
              match Hashtbl.find_opt classical chan with
              | Some ((xb, _) as r) ->
                  push
                    (Ir.WaitC
                       {
                         chan;
                         desc =
                           Printf.sprintf "receive %s := %s (classical channel %s)" arg xb chan;
                       });
                  emit ((arg, `Res r) :: env) k
              | None ->
                  push
                    (Ir.WaitC
                       { chan; desc = Printf.sprintf "wait on channel %s (synchronizer)" chan });
                  emit env k
            end
        | Choice (l, _r) -> emit env l (* codegen commits left; the machine explores both *)
        | Replication r -> emit env r (* one local copy *)
        | ProcessRef n ->
            push (Ir.Note (Printf.sprintf "ERROR: unresolved process reference %s" n))
      in
      List.iter (emit []) (membrane_processes m);
      List.rev !out
    in
    let procs = List.mapi (fun i m -> { Ir.mem = i; body = lower_membrane i m }) membranes in
    { Ir.nqubits; cregs = !cregs; procs; regions }
  in
  let rec fix prev =
    let c = lower_round () in
    let cur = Hashtbl.length classical in
    if cur > prev then fix cur else c
  in
  fix (-1)

let compile_to_qasm (membranes : membrane list) : string =
  Ir.to_qasm (lower membranes)
