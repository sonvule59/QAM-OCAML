(* ============================================================================
   Phase 5: property-based verification.

   A small deterministic property harness (seeded Random.State, no external
   dependency): each property runs over hundreds of generated configurations;
   a failure reports the iteration index and the offending configuration.

   Two generators:
   - [gen_config]: arbitrary small configurations (often ill-typed) — used for
     totality/safety and for machine invariants;
   - [gen_teleport_variant]: randomized teleportation protocols (fresh names,
     shuffled molecules, random membrane order, junk molecules, optional
     Choice wrapping) — used for the tested Theorem-28 simulation relation:
     every variant must typecheck, physically teleport when compiled and
     simulated, and abstractly teleport in the machine.
   ========================================================================== *)

open OUnit2
open Ast

let seed k = Random.State.make [| 0x9a55ed; k |]
let pick st arr = arr.(Random.State.int st (Array.length arr))

let shuffle st l =
  let a = Array.of_list l in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int st (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  Array.to_list a

let repeat n f =
  for k = 0 to n - 1 do
    f k
  done

let string_of_cfg = Semantics.string_of_config

(* ---- generator: arbitrary small configurations --------------------------- *)

let names = [| "a"; "b"; "c"; "d" |]

let gen_act st =
  let n () = pick st names in
  match Random.State.int st 5 with
  | 0 -> NewChannel (n ())
  | 1 -> Send { chan = n (); arg = Named (n ()) }
  | 2 -> Receive { chan = n (); arg = n () }
  | 3 -> Encode { chan = n (); msg = Named (n ()) }
  | _ -> Decode { chan = n (); binder = n () }

let rec gen_proc st depth =
  if depth = 0 then NullProcess
  else
    match Random.State.int st 5 with
    | 0 | 1 -> ActionProcess (mk_action (gen_act st), gen_proc st (depth - 1))
    | 2 -> Choice (gen_proc st (depth - 1), gen_proc st (depth - 1))
    | 3 -> Replication (gen_proc st (depth - 1))
    | _ -> NullProcess

let gen_res st =
  match Random.State.int st 3 with
  | 0 -> NullResource
  | 1 -> SimpleResource (pick st names)
  | _ -> ChannelParty (pick st names, Blank)

let gen_molecule st =
  if Random.State.bool st then ProcessMolecule (gen_proc st (1 + Random.State.int st 2))
  else ResourceMolecule (gen_res st)

let gen_config st =
  List.init
    (1 + Random.State.int st 2)
    (fun _ -> MoleculeMembrane (List.init (1 + Random.State.int st 3) (fun _ -> gen_molecule st)))

(* ---- property: totality — nothing raises on arbitrary input -------------- *)

let test_prop_totality _ =
  let st = seed 1 in
  repeat 200 (fun k ->
      let cfg = gen_config st in
      try
        ignore (Semantics.step cfg);
        (match Typecheck.check cfg with Ok _ | Error _ -> ());
        ignore (Compile.compile_to_qasm cfg)
      with e ->
        assert_failure
          (Printf.sprintf "iteration %d raised %s on: %s" k (Printexc.to_string e)
             (string_of_cfg cfg)))

(* ---- property: dynamic no-cloning invariants ------------------------------
   Along every reachable configuration (bounded exploration):
   - the number of resource molecules never increases across a step
     (Cohere converts blanks 2->2; Encode/Decode/CLocal consume);
   - no channel ever has more than two parties, provided it started with at
     most two (paper Def. 23 / Thm. 3). *)

let resource_count cfg =
  List.fold_left
    (fun acc m ->
      match m with
      | MoleculeMembrane ms ->
          acc
          + List.length (List.filter (function ResourceMolecule _ -> true | _ -> false) ms)
      | _ -> acc)
    0 cfg

let parties_per_channel cfg =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun m ->
      match m with
      | MoleculeMembrane ms ->
          List.iter
            (function
              | ResourceMolecule (ChannelParty (c, _)) ->
                  Hashtbl.replace tbl c (1 + Option.value ~default:0 (Hashtbl.find_opt tbl c))
              | _ -> ())
            ms
      | _ -> ())
    cfg;
  tbl

let max_parties cfg =
  Hashtbl.fold (fun _ n acc -> max n acc) (parties_per_channel cfg) 0

let test_prop_no_cloning_invariants _ =
  let st = seed 2 in
  repeat 80 (fun k ->
      let cfg0 = gen_config st in
      if max_parties cfg0 <= 2 then begin
        let seen = Hashtbl.create 64 in
        let rec explore d cfg =
          if d > 0 then
            let key = Semantics.config_key cfg in
            if not (Hashtbl.mem seen key) then begin
              Hashtbl.replace seen key ();
              let rc = resource_count cfg in
              List.iter
                (fun (_, cfg') ->
                  if resource_count cfg' > rc then
                    assert_failure
                      (Printf.sprintf
                         "iteration %d: resource count grew (cloning!) from %s to %s" k
                         (string_of_cfg cfg) (string_of_cfg cfg'));
                  if max_parties cfg' > 2 then
                    assert_failure
                      (Printf.sprintf "iteration %d: >2 parties for a channel in %s" k
                         (string_of_cfg cfg'));
                  explore (d - 1) cfg')
                (Semantics.step cfg)
            end
        in
        explore 5 cfg0
      end)

(* ---- property: trace sets are prefix-closed (Def. 14) and refinement is
   reflexive (Def. 15) ------------------------------------------------------ *)

let test_prop_traces_prefix_closed _ =
  let st = seed 3 in
  repeat 60 (fun k ->
      let cfg = gen_config st in
      let ts = Semantics.traces ~depth:4 cfg in
      let set = Hashtbl.create 16 in
      List.iter (fun t -> Hashtbl.replace set t ()) ts;
      List.iter
        (fun t ->
          if t <> "" then
            let prefix =
              match String.rindex_opt t ';' with
              | Some i -> String.sub t 0 i
              | None -> ""
            in
            if not (Hashtbl.mem set prefix) then
              assert_failure
                (Printf.sprintf "iteration %d: trace %S lacks prefix %S for %s" k t prefix
                   (string_of_cfg cfg)))
        ts)

let test_prop_refinement_reflexive _ =
  let st = seed 4 in
  repeat 40 (fun k ->
      let cfg = gen_config st in
      if not (Semantics.trace_refines ~depth:4 cfg cfg) then
        assert_failure (Printf.sprintf "iteration %d: not reflexive: %s" k (string_of_cfg cfg)))

(* ---- generator: randomized teleportation variants ------------------------- *)

type variant = { cfg : membrane list; msg_name : string }

let gen_teleport_variant st : variant =
  let n = ref 0 in
  let fresh p =
    incr n;
    Printf.sprintf "%s%d" p !n
  in
  let c = fresh "c" and a = fresh "a" and d = fresh "d" in
  let x = fresh "x" and u = fresh "u" and z = fresh "z" in
  let act a k = ActionProcess (mk_action a, k) in
  let alice_proto =
    act (NewChannel c)
      (act (Encode { chan = c; msg = Named d })
         (act (Decode { chan = c; binder = x })
            (act (Send { chan = a; arg = Named x }) NullProcess)))
  in
  let bob_proto =
    act (NewChannel c)
      (act (Receive { chan = c; arg = u })
         (act (Receive { chan = a; arg = z })
            (act (Encode { chan = u; msg = Named z }) NullProcess)))
  in
  (* optional junk: inert molecules and a junk Choice branch *)
  let junk_molecule () =
    match Random.State.int st 3 with
    | 0 -> ResourceMolecule NullResource
    | 1 -> ResourceMolecule (SimpleResource (fresh "r"))
    | _ ->
        ProcessMolecule
          (act (Send { chan = fresh "j"; arg = Named (fresh "k") }) NullProcess)
  in
  let junks () = List.init (Random.State.int st 3) (fun _ -> junk_molecule ()) in
  let maybe_choice p =
    if Random.State.int st 3 = 0 then
      Choice (p, act (Send { chan = fresh "j"; arg = Named (fresh "k") }) NullProcess)
    else p
  in
  let alice =
    MoleculeMembrane
      (shuffle st
         ([ ProcessMolecule (maybe_choice alice_proto);
            ResourceMolecule (SimpleResource d);
            ResourceMolecule NullResource ]
         @ junks ()))
  in
  let bob =
    MoleculeMembrane
      (shuffle st ([ ProcessMolecule bob_proto; ResourceMolecule NullResource ] @ junks ()))
  in
  let cfg = if Random.State.bool st then [ alice; bob ] else [ bob; alice ] in
  { cfg; msg_name = d }

(* ---- property: every variant typechecks ----------------------------------- *)

let test_prop_variants_well_typed _ =
  let st = seed 5 in
  repeat 60 (fun k ->
      let v = gen_teleport_variant st in
      match Typecheck.check v.cfg with
      | Ok _ -> ()
      | Error (e :: _) ->
          assert_failure
            (Printf.sprintf "iteration %d: variant ill-typed (%s): %s" k
               (Typecheck.string_of_error e) (string_of_cfg v.cfg))
      | Error [] -> assert_failure "empty error list")

(* ---- property: Theorem 28, tested — circuit side --------------------------
   Compile each variant and SIMULATE it; the wiring (measured qubits, Bob's
   corrected qubit, the message qubit) is derived from the emitted circuit
   itself, so this is robust to shuffling, junk and dead-qubit compaction. *)

let check_physics k (v : variant) =
  let qasm = Compile.compile_to_qasm v.cfg in
  let _, instrs = Sim.parse qasm in
  let meas = List.filter_map (function Sim.Measure (q, r) -> Some (r, q) | _ -> None) instrs in
  let bob = List.find_map (function Sim.IfX (_, q) -> Some q | _ -> None) instrs in
  match (bob, List.assoc_opt "m1" meas) with
  | Some bob, Some msgq ->
      let alpha = 0.6 and beta = 0.8 in
      let branches =
        Sim.run ~init:(msgq, ({ Complex.re = alpha; im = 0.0 }, { Complex.re = beta; im = 0.0 }))
          qasm
      in
      if List.length branches <> 4 then
        assert_failure
          (Printf.sprintf "iteration %d: expected 4 branches, got %d for %s" k
             (List.length branches) (string_of_cfg v.cfg));
      List.iter
        (fun br ->
          let bit r = match List.assoc_opt r br.Sim.cregs with Some b -> b | None -> 0 in
          let base =
            List.fold_left
              (fun acc (r, q) -> if bit r = 1 then acc lor (1 lsl q) else acc)
              0 meas
          in
          let a0 = br.Sim.amp.(base) and a1 = br.Sim.amp.(base lor (1 lsl bob)) in
          let p = Complex.norm2 a0 +. Complex.norm2 a1 in
          if Float.abs (p -. 0.25) > 1e-9 then
            assert_failure
              (Printf.sprintf "iteration %d: branch probability %f <> 1/4 for %s" k p
                 (string_of_cfg v.cfg));
          let cross =
            Complex.sub
              (Complex.mul a0 { Complex.re = beta; im = 0.0 })
              (Complex.mul a1 { Complex.re = alpha; im = 0.0 })
          in
          if Complex.norm cross > 1e-9 then
            assert_failure
              (Printf.sprintf "iteration %d: message state not teleported for %s" k
                 (string_of_cfg v.cfg)))
        branches
  | _ ->
      assert_failure
        (Printf.sprintf "iteration %d: compiled circuit lacks teleport structure for %s" k
           (string_of_cfg v.cfg))

let test_prop_variants_teleport_physically _ =
  let st = seed 6 in
  repeat 40 (fun k -> check_physics k (gen_teleport_variant st))

(* ---- property: Theorem 28, tested — machine side ---------------------------
   The abstract machine must also deliver: some reachable terminal has the
   receiver holding the message as a channel party d.◦. Circuit and machine
   agreeing on this observable across randomized programs is the simulation
   relation of Theorem 28, tested rather than proved. *)

let test_prop_variants_teleport_abstractly _ =
  let st = seed 7 in
  repeat 20 (fun k ->
      let v = gen_teleport_variant st in
      let terms = Semantics.reachable_terminals ~depth:18 v.cfg in
      let delivered =
        List.exists
          (fun (cfg, _) ->
            List.exists
              (function
                | MoleculeMembrane ms ->
                    List.mem (ResourceMolecule (ChannelParty (v.msg_name, Blank))) ms
                | _ -> false)
              cfg)
          terms
      in
      if not delivered then
        assert_failure
          (Printf.sprintf "iteration %d: machine never delivers %s for %s" k v.msg_name
             (string_of_cfg v.cfg)))

let suite =
  "property_tests"
  >::: [
         "prop_totality" >:: test_prop_totality;
         "prop_no_cloning_invariants" >:: test_prop_no_cloning_invariants;
         "prop_traces_prefix_closed" >:: test_prop_traces_prefix_closed;
         "prop_refinement_reflexive" >:: test_prop_refinement_reflexive;
         "prop_variants_well_typed" >:: test_prop_variants_well_typed;
         "prop_variants_teleport_physically" >:: test_prop_variants_teleport_physically;
         "prop_variants_teleport_abstractly" >:: test_prop_variants_teleport_abstractly;
       ]

let () = run_test_tt_main suite
