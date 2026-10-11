open Ast

(* ============================================================================
   The QAM machine (Phase 2): a configuration-level labeled transition system
   implementing the reduction semantics of Li et al., "The Quantum Abstract
   Machine" (arXiv:2402.13469), Figures 5-6 and Section 3.

   A configuration is a multiset of membranes (here: a list). One step is
     step : membrane list -> (label * membrane list) list
   returning EVERY successor (a real LTS: Choice takes both branches, every
   redex position fires), with the paper's labels on Cohere, Decode and Com.

   Rules implemented:
     Cohere  {|nu c.R, ◦, M|}, {|nu c.T, ◦, N|} --c--> {|R, c.◦, M|}, {|T, c.◦, N|}
     Encode  {|a<-mu1.R, a.mu2, ...|} --> {|R, a.(mu1 ⊙ mu2), ...|}, with QLocal
             fused: a Named message naming a resource in the membrane is
             localized (consumed) first.
     Decode  fused with AirEQ: decoder and receiver membranes each holding a
             party of c pool their content (mu = mu1 ⊙ mu2); the decoder binds
             the classical residue ^c.mu, the receiver binds the projective
             channel ^c and gains the quantum residue ^c.~mu.
     CLocal  {|^c?(x).R, ^c.i, ...|} --> {|R[i/x], ...|}
     Com     {|a!i.R, ...|}, {|a?(x).T, ...|} --a.i--> {|R, ...|}, {|T[i/x], ...|}
     CL/CR   R + T --> R and R + T --> T (both successors)
     MT/NT   !R --> R, !R (nu-binders freshened) and !R --> 0
     Split   {|M, N|} --> {|M|}, {|N|}      (off by default: state explosion)
     Decohere phi --> ∅                     (off by default: state explosion)
     ID1/ID2 applied as tidying: null processes and empty membranes vanish.

   The meet algebra ⊙ (Def. 5) is implemented as directed rewrites in
   [meet_normalize]. Projective channels are ordinary names prefixed "^".

   Trace semantics and refinement (Defs. 14-15) are computed over a bounded
   number of steps (`depth`); with replication the LTS is infinite, so the
   bound is a documented approximation.
   ========================================================================== *)

type label =
  | Tau
  | LChan of string * (int * int)         (* Cohere raises c; the two membranes *)
  | LDecode of string * msg * (int * int) (* Decode raises ^c.mu *)
  | LCom of string * msg * (int * int)    (* Com raises a.i *)

type rule =
  | RCohere | RCom | RDecode | REncode | RCLocal | RChoice | RRepl
  | RSplit | RDecohere

let default_rules = [ RCohere; RCom; RDecode; REncode; RCLocal; RChoice; RRepl ]

let string_of_label = function
  | Tau -> "tau"
  | LChan (c, _) -> c
  | LDecode (c, m, _) -> "^" ^ c ^ "." ^ Interpreter.string_of_msg m
  | LCom (a, m, _) -> a ^ "." ^ Interpreter.string_of_msg m

let is_projective n = String.length n > 0 && n.[0] = '^'
let projective c = "^" ^ c

(* ---- the meet algebra ⊙ (Def. 5), as directed rewrites ------------------- *)

(* approximation of the paper's classical messages i: projective-labeled or
   residue content *)
let rec classicalish = function
  | Channeled (c, _) -> is_projective c
  | Projected _ -> true
  | Residue m -> classicalish m
  | _ -> false

let rec meet_normalize (m : msg) : msg =
  match m with
  | Meet (a, b) -> (
      let a = meet_normalize a and b = meet_normalize b in
      match (a, b) with
      | Blank, x | x, Blank -> x (* ◦ ⊙ q ≡ q *)
      | Channeled (c1, q1), Channeled (c2, Residue q2)
        when is_projective c1 && c1 = c2 && q1 = q2 ->
          q1 (* ^c.q ⊙ ^c.~q ≡ q : the key-lock recovery *)
      | Channeled (c2, Residue q2), Channeled (c1, q1)
        when is_projective c1 && c1 = c2 && q1 = q2 ->
          q1
      | x, y when x = y && classicalish x -> Blank (* i ⊙ i ≡ ◦ *)
      | x, y -> if compare x y <= 0 then Meet (x, y) else Meet (y, x))
  | Channeled (c, inner) -> (
      let inner = meet_normalize inner in
      match inner with
      (* Classical key-lock keeps the projective label, per the paper's own
         Example 21 (^c.(^c.i ⊙ ~i) ≡ ^c.i, then CLocal delivers i), whereas a
         quantum payload (a channel party) sheds it, per Example 8. Def. 5
         leaves this distinction implicit; we follow the worked examples. *)
      | Meet (Channeled (c2, i), Residue i2) | Meet (Residue i2, Channeled (c2, i))
        when is_projective c && c2 = c && i = i2
             && (match i with Channeled _ -> false | _ -> true) ->
          Channeled (c, i)
      (* nested projective rearrangement: ^c.(^c.i ⊙ mu) ≡ ^c.i ⊙ ^c.mu *)
      | Meet (Channeled (c2, i), mu) when is_projective c && c2 = c ->
          meet_normalize (Meet (Channeled (c2, i), Channeled (c, mu)))
      | Meet (mu, Channeled (c2, i)) when is_projective c && c2 = c ->
          meet_normalize (Meet (Channeled (c2, i), Channeled (c, mu)))
      | _ -> Channeled (c, inner))
  | Residue m -> Residue (meet_normalize m)
  | Projected (c, m) -> Projected (c, meet_normalize m)
  | Blank | Named _ -> m

(* ---- resources <-> message values ---------------------------------------- *)

(* a resource seen as the paper's phi ::= alpha.mu | ◦ *)
let party_of = function
  | ChannelParty (c, m) -> Some (c, m)
  | SimpleResource n -> Some (n, Blank) (* bare named resource ~ n.◦ *)
  | _ -> None

let msg_to_resource = function
  | Blank -> NullResource
  | Named n -> SimpleResource n
  | Channeled (c, m) -> ChannelParty (c, m)
  | m -> ChannelParty ("?", m) (* does not arise in the supported fragment *)

(* ---- substitution R[v/x] -------------------------------------------------- *)

let subst_var (x : string) (v : msg) (p : process) : process =
  let vname = match v with Named n -> Some n | _ -> None in
  let sub_chan c =
    if c = x then match vname with Some n -> n | None -> c else c
  in
  let rec sub_msg m =
    match m with
    | Named n when n = x -> v
    | Named _ | Blank -> m
    | Channeled (c, mm) -> Channeled (sub_chan c, sub_msg mm)
    | Projected (c, mm) -> Projected (sub_chan c, sub_msg mm)
    | Residue mm -> Residue (sub_msg mm)
    | Meet (a, b) -> Meet (sub_msg a, sub_msg b)
  in
  let rec go p =
    match p with
    | NullProcess | ProcessRef _ -> p
    | Choice (a, b) -> Choice (go a, go b)
    | Replication r -> Replication (go r)
    | ActionProcess ({ act; loc }, k) ->
        let shadowed, act' =
          match act with
          | NewChannel c -> (c = x, NewChannel c)
          | Send { chan; arg } -> (false, Send { chan = sub_chan chan; arg = sub_msg arg })
          | Receive { chan; arg } -> (arg = x, Receive { chan = sub_chan chan; arg })
          | Encode { chan; msg } -> (false, Encode { chan = sub_chan chan; msg = sub_msg msg })
          | Decode { chan; binder } -> (binder = x, Decode { chan = sub_chan chan; binder })
        in
        ActionProcess ({ act = act'; loc }, if shadowed then k else go k)
  in
  go p

(* ---- soup helpers --------------------------------------------------------- *)

let mols = function MoleculeMembrane ms -> Some ms | _ -> None

(* all ways to remove one element (order of the rest preserved) *)
let pick xs =
  let rec go before = function
    | [] -> []
    | x :: after -> (x, List.rev_append before after) :: go (x :: before) after
  in
  go [] xs

(* prefixes: molecules of the form ActionProcess at top *)
let prefixes ms =
  List.filter_map
    (function
      | ProcessMolecule (ActionProcess (a, k)), rest -> Some (a, k, rest)
      | _ -> None)
    (pick ms)

let find_blank rest =
  List.find_map
    (function
      | ResourceMolecule NullResource, rest' -> Some rest'
      | _ -> None)
    (pick rest)

let find_party c rest =
  List.find_map
    (fun (m, rest') ->
      match m with
      | ResourceMolecule r -> (
          match party_of r with
          | Some (c', mu) when c' = c -> Some (mu, rest')
          | _ -> None)
      | _ -> None)
    (pick rest)

(* ID1/ID2: null processes and empty membranes vanish. [keep_empty] keeps
   empty membranes in place so membrane POSITIONS stay stable (the extended
   QAM attaches locations by position; see network.ml). *)
let tidy ?(keep_empty = false) (cfg : membrane list) : membrane list =
  cfg
  |> List.map (fun m ->
         match m with
         | MoleculeMembrane ms ->
             MoleculeMembrane
               (List.filter (function ProcessMolecule NullProcess -> false | _ -> true) ms)
         | m -> m)
  |> fun cfg' ->
  if keep_empty then cfg'
  else List.filter (function MoleculeMembrane [] -> false | _ -> true) cfg'

(* replace membrane i's molecules *)
let set_at cfg i ms =
  List.mapi (fun j m -> if j = i then MoleculeMembrane ms else m) cfg

(* ---- one transition step -------------------------------------------------- *)

let step ?(rules = default_rules) ?(keep_empty = false) (cfg : membrane list) :
    (label * membrane list) list =
  let has r = List.mem r rules in
  let out = ref [] in
  let add l cfg' = out := (l, tidy ~keep_empty cfg') :: !out in
  let indexed =
    List.mapi (fun i m -> (i, m)) cfg
    |> List.filter_map (fun (i, m) ->
           match mols m with Some ms -> Some (i, ms) | None -> None)
  in
  (* --- intra-membrane rules --- *)
  List.iter
    (fun (i, ms) ->
      (* Encode (with QLocal fused) *)
      (if has REncode then
         List.iter
           (fun (a, k, rest) ->
             match a.act with
             | Encode { chan = alpha; msg } -> (
                 match find_party alpha rest with
                 | Some (mu2, rest') ->
                     let mu1, rest'' =
                       match msg with
                       | Named n -> (
                           match find_party n rest' with
                           | Some (mn, r2) -> (Channeled (n, mn), r2) (* QLocal *)
                           | None -> (msg, rest'))
                       | m -> (m, rest')
                     in
                     let v = meet_normalize (Channeled (alpha, Meet (mu1, mu2))) in
                     add Tau
                       (set_at cfg i
                          (ProcessMolecule k :: ResourceMolecule (msg_to_resource v) :: rest''))
                 | None -> ())
             | _ -> ())
           (prefixes ms));
      (* CLocal *)
      (if has RCLocal then
         List.iter
           (fun (a, k, rest) ->
             match a.act with
             | Receive { chan; arg } when is_projective chan -> (
                 match find_party chan rest with
                 | Some (iota, rest') ->
                     add Tau
                       (set_at cfg i (ProcessMolecule (subst_var arg iota k) :: rest'))
                 | None -> ())
             | _ -> ())
           (prefixes ms));
      (* CL / CR: both branches *)
      (if has RChoice then
         List.iter
           (fun (m, rest) ->
             match m with
             | ProcessMolecule (Choice (l, r)) ->
                 add Tau (set_at cfg i (ProcessMolecule l :: rest));
                 add Tau (set_at cfg i (ProcessMolecule r :: rest))
             | _ -> ())
           (pick ms));
      (* MT / NT *)
      (if has RRepl then
         List.iter
           (fun (m, rest) ->
             match m with
             | ProcessMolecule (Replication p) ->
                 let used =
                   List.concat_map
                     (function ProcessMolecule q -> names_of_process q | _ -> [])
                     ms
                 in
                 add Tau
                   (set_at cfg i
                      (ProcessMolecule (Interpreter.freshen_nus used p)
                      :: ProcessMolecule (Replication p) :: rest));
                 add Tau (set_at cfg i rest) (* NT: !R --> 0 *)
             | _ -> ())
           (pick ms));
      (* Split (flagged) *)
      (if has RSplit && List.length ms >= 2 then
         List.iter
           (fun (m, rest) ->
             let cfg' =
               List.concat
                 (List.mapi
                    (fun j mem ->
                      if j = i then [ MoleculeMembrane [ m ]; MoleculeMembrane rest ]
                      else [ mem ])
                    cfg)
             in
             add Tau cfg')
           (pick ms));
      (* Decohere (flagged) *)
      if has RDecohere then
        List.iter
          (fun (m, rest) ->
            match m with
            | ResourceMolecule _ -> add Tau (set_at cfg i rest)
            | _ -> ())
          (pick ms))
    indexed;
  (* --- inter-membrane rules --- *)
  List.iter
    (fun (i, ms_i) ->
      List.iter
        (fun (j, ms_j) ->
          if i <> j then begin
            (* Com: sender in i, receiver in j *)
            (if has RCom then
               List.iter
                 (fun (ai, ri, rest_i) ->
                   match ai.act with
                   | Send { chan = a; arg = iota } ->
                       List.iter
                         (fun (aj, tj, rest_j) ->
                           match aj.act with
                           | Receive { chan = a'; arg = y } when a' = a ->
                               let cfg' = set_at cfg i (ProcessMolecule ri :: rest_i) in
                               let cfg' =
                                 set_at cfg' j
                                   (ProcessMolecule (subst_var y iota tj) :: rest_j)
                               in
                               add (LCom (a, iota, (i, j))) cfg'
                           | _ -> ())
                         (prefixes ms_j)
                   | _ -> ())
                 (prefixes ms_i));
            (* Decode: decoder in i, receiver in j; both hold a party of c *)
            (if has RDecode then
               List.iter
                 (fun (ai, ri, rest_i) ->
                   match ai.act with
                   | Decode { chan = c; binder = x } -> (
                       match find_party c rest_i with
                       | Some (mu1, rest_i') ->
                           List.iter
                             (fun (aj, tj, rest_j) ->
                               match aj.act with
                               | Receive { chan = c'; arg = y } when c' = c -> (
                                   match find_party c rest_j with
                                   | Some (mu2, rest_j') ->
                                       let mu = meet_normalize (Meet (mu1, mu2)) in
                                       let pc = projective c in
                                       let cfg' =
                                         set_at cfg i
                                           (ProcessMolecule
                                              (subst_var x (Channeled (pc, mu)) ri)
                                           :: rest_i')
                                       in
                                       let cfg' =
                                         set_at cfg' j
                                           (ProcessMolecule (subst_var y (Named pc) tj)
                                           :: ResourceMolecule
                                                (ChannelParty (pc, Residue mu))
                                           :: rest_j')
                                       in
                                       add (LDecode (c, mu, (i, j))) cfg'
                                   | None -> ())
                               | _ -> ())
                             (prefixes ms_j)
                       | None -> ())
                   | _ -> ())
                 (prefixes ms_i));
            (* Cohere: nu c. in both membranes, a blank in each (i < j once) *)
            if has RCohere && i < j then
              List.iter
                (fun (ai, ri, rest_i) ->
                  match ai.act with
                  | NewChannel c -> (
                      match find_blank rest_i with
                      | Some rest_i' ->
                          List.iter
                            (fun (aj, tj, rest_j) ->
                              match aj.act with
                              | NewChannel c' when c' = c -> (
                                  match find_blank rest_j with
                                  | Some rest_j' ->
                                      let cfg' =
                                        set_at cfg i
                                          (ProcessMolecule ri
                                          :: ResourceMolecule (ChannelParty (c, Blank))
                                          :: rest_i')
                                      in
                                      let cfg' =
                                        set_at cfg' j
                                          (ProcessMolecule tj
                                          :: ResourceMolecule (ChannelParty (c, Blank))
                                          :: rest_j')
                                      in
                                      add (LChan (c, (i, j))) cfg'
                                  | None -> ())
                              | _ -> ())
                            (prefixes ms_j)
                      | None -> ())
                  | _ -> ())
                (prefixes ms_i)
          end)
        indexed)
    indexed;
  List.rev !out

(* ---- exploration: traces, terminals, refinement --------------------------- *)

let string_of_config (cfg : membrane list) : string =
  match cfg with
  | [] -> "(empty)"
  | _ -> String.concat ", " (List.map Interpreter.string_of_membrane cfg)

(* canonical key: membranes as sorted multisets of printed molecules,
   configuration as a sorted multiset of membranes *)
let config_key (cfg : membrane list) : string =
  tidy cfg
  |> List.map (fun m ->
         match m with
         | MoleculeMembrane ms ->
             "{"
             ^ String.concat ","
                 (List.sort compare (List.map Interpreter.string_of_molecule ms))
             ^ "}"
         | m -> Interpreter.string_of_membrane m)
  |> List.sort compare |> String.concat "|"

module SS = Set.Make (String)

(* the bounded trace set T(C) (Def. 14): sequences of visible labels along runs
   of at most [depth] steps, prefix-closed; tau steps consume depth only *)
let traces ?rules ~depth (cfg : membrane list) : string list =
  let memo : (string * int, SS.t) Hashtbl.t = Hashtbl.create 256 in
  let rec suf d cfg =
    if d = 0 then SS.singleton ""
    else
      let key = (config_key cfg, d) in
      match Hashtbl.find_opt memo key with
      | Some s -> s
      | None ->
          let s =
            List.fold_left
              (fun acc (l, cfg') ->
                let s' = suf (d - 1) cfg' in
                let s' =
                  match l with
                  | Tau -> s'
                  | _ ->
                      let ls = string_of_label l in
                      SS.map (fun t -> if t = "" then ls else ls ^ ";" ^ t) s'
                in
                SS.union acc s')
              (SS.singleton "")
              (step ?rules cfg)
          in
          Hashtbl.replace memo key s;
          s
  in
  SS.elements (suf depth cfg)

(* bounded trace refinement (Def. 15): T(c1) ⊆ T(c2), both to [depth] steps *)
let trace_refines ?rules ~depth c1 c2 : bool =
  let t2 = SS.of_list (traces ?rules ~depth c2) in
  List.for_all (fun t -> SS.mem t t2) (traces ?rules ~depth c1)

(* ---- exact (unbounded) traces for the replication-free fragment ----------
   Without replication every rule strictly consumes (a prefix, a branch, a
   molecule or a resource), so the LTS is finite and exploration terminates:
   trace refinement is EXACT there, no depth bound needed. With `repl` the
   LTS is infinite-state and only the bounded approximation is computable. *)

let rec process_has_repl = function
  | Replication _ -> true
  | ActionProcess (_, k) -> process_has_repl k
  | Choice (a, b) -> process_has_repl a || process_has_repl b
  | NullProcess | ProcessRef _ -> false

let has_replication (cfg : membrane list) : bool =
  List.exists
    (function
      | MoleculeMembrane ms ->
          List.exists
            (function ProcessMolecule p -> process_has_repl p | _ -> false)
            ms
      | _ -> false)
    cfg

let traces_exact ?rules (cfg : membrane list) : string list =
  if has_replication cfg then
    invalid_arg "traces_exact: configuration uses repl; use traces ~depth";
  let memo : (string, SS.t) Hashtbl.t = Hashtbl.create 256 in
  let rec suf cfg =
    let key = config_key cfg in
    match Hashtbl.find_opt memo key with
    | Some s -> s
    | None ->
        let s =
          List.fold_left
            (fun acc (l, cfg') ->
              let s' = suf cfg' in
              let s' =
                match l with
                | Tau -> s'
                | _ ->
                    let ls = string_of_label l in
                    SS.map (fun t -> if t = "" then ls else ls ^ ";" ^ t) s'
              in
              SS.union acc s')
            (SS.singleton "")
            (step ?rules cfg)
        in
        Hashtbl.replace memo key s;
        s
  in
  SS.elements (suf cfg)

(* exact trace refinement for replication-free configurations *)
let trace_refines_exact ?rules c1 c2 : bool =
  let t2 = SS.of_list (traces_exact ?rules c2) in
  List.for_all (fun t -> SS.mem t t2) (traces_exact ?rules c1)

(* terminal (stuck) configurations reachable within [depth] steps, each with a
   witness sequence of visible labels *)
let reachable_terminals ?rules ~depth (cfg : membrane list) :
    (membrane list * string list) list =
  let seen : (string, int) Hashtbl.t = Hashtbl.create 256 in
  let terms : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let out = ref [] in
  let rec go d cfg tr =
    let key = config_key cfg in
    let revisit = match Hashtbl.find_opt seen key with Some d' -> d > d' | None -> true in
    if revisit then begin
      Hashtbl.replace seen key d;
      match step ?rules cfg with
      | [] ->
          if not (Hashtbl.mem terms key) then begin
            Hashtbl.replace terms key ();
            out := (tidy cfg, List.rev tr) :: !out
          end
      | succs ->
          if d > 0 then
            List.iter
              (fun (l, c') ->
                go (d - 1) c' (match l with Tau -> tr | l -> string_of_label l :: tr))
              succs
    end
  in
  go depth cfg [];
  List.rev !out
