open Ast

(* ============================================================================
   Phase 6: the EXTENDED QAM (paper Section 4) — quantum-network evaluation.

   Membranes get locations {|M|}g (attached by POSITION in the configuration,
   so the machine runs with keep_empty so positions stay stable); theta is the
   success-rate graph over locations; xi (here [intent]) maps each channel to
   the (source, destination) its creation intends to connect.

   The extended judgment P --p(g,h)--> Q is implemented by filtering the core
   machine's labeled transitions (rules Trans / Chan / Msg, Sec. 4.1):
   - Chan : a Cohere on channel c between membranes at locations (g,h) is
     valid iff the predicate phi admits the edge, and raises rate theta(g,h);
   - Msg  : a Decode transition (ownership transfer in QES) raises rate 1 and
     is validated against the transferred channel's intention;
   - Trans: every other transition passes through unchanged.

   phi is instantiated by routing policy (Sec. 4.2):
   - QPass: the edge must lie on THE shortest path from source to destination;
   - QCast: on the path maximizing the SUM of assumed success rates (the
     paper's Example 13 numbers select Ann->Rb->Rc->Bob, confirming "sum");
   - AnyEdge: only requires the edge to exist in theta.

   Likeliness traces and refinement follow Defs. 16-19: traces are sequences
   of p(g,h) labels; sigma1 <= sigma2 pointwise on rates with equal endpoints;
   LQTR is bounded-depth trace subset under that order.
   ========================================================================== *)

type location = string

type net = {
  locs : location array; (* membrane position -> location *)
  theta : ((location * location) * float) list; (* undirected success rates *)
  intent : (string * (location * location)) list; (* xi: channel -> (src, dst) *)
}

type policy = AnyEdge | QPass | QCast

let rate net g h =
  List.find_map
    (fun ((a, b), p) -> if (a = g && b = h) || (a = h && b = g) then Some p else None)
    net.theta

let neighbors net g =
  List.concat_map
    (fun ((a, b), _) -> if a = g then [ b ] else if b = g then [ a ] else [])
    net.theta
  |> List.sort_uniq compare

(* all simple paths src..dst (inclusive); networks are small graphs *)
let simple_paths net src dst =
  let rec go visited g =
    if g = dst then [ [ g ] ]
    else
      neighbors net g
      |> List.filter (fun n -> not (List.mem n visited))
      |> List.concat_map (fun n -> List.map (fun p -> g :: p) (go (n :: visited) n))
  in
  go [ src ] src

let path_sum net p =
  let rec go = function
    | a :: (b :: _ as tl) -> Option.value ~default:0.0 (rate net a b) +. go tl
    | _ -> 0.0
  in
  go p

(* the path a policy commits to for an intention, deterministically tie-broken *)
let chosen_path policy net (src, dst) : location list option =
  match simple_paths net src dst with
  | [] -> None
  | ps -> (
      match policy with
      | AnyEdge -> None
      | QPass ->
          Some (List.hd (List.sort (fun a b -> compare (List.length a, a) (List.length b, b)) ps))
      | QCast ->
          Some
            (List.hd
               (List.sort
                  (fun a b ->
                    compare
                      (-.path_sum net a, List.length a, a)
                      (-.path_sum net b, List.length b, b))
                  ps)))

let edge_on_path p (g, h) =
  let rec go = function
    | a :: (b :: _ as tl) -> ((a = g && b = h) || (a = h && b = g)) || go tl
    | _ -> false
  in
  go p

(* phi for rule Chan: the edge must lie on the policy's chosen path for the
   channel's intention (channels without a registered intention are free) *)
let phi_edge policy net chan (g, h) =
  match (policy, List.assoc_opt chan net.intent) with
  | AnyEdge, _ | _, None -> true
  | pol, Some sd -> (
      match chosen_path pol net sd with Some p -> edge_on_path p (g, h) | None -> false)

(* phi for rule Msg: the locations involved in the transfer must lie ON the
   chosen path (the paper validates QES decodes against path membership) *)
let phi_nodes policy net chan gs =
  match (policy, List.assoc_opt chan net.intent) with
  | AnyEdge, _ | _, None -> true
  | pol, Some sd -> (
      match chosen_path pol net sd with
      | Some p -> List.for_all (fun g -> List.mem g p) gs
      | None -> false)

(* ---- the extended transition system --------------------------------------- *)

type nlabel = NPlain of Semantics.label | NRate of float * location * location

let string_of_nlabel = function
  | NPlain l -> Semantics.string_of_label l
  | NRate (p, g, h) -> Printf.sprintf "%.2f(%s,%s)" p g h

let loc net i = if i < Array.length net.locs then net.locs.(i) else "?"

(* a decoded message carries a LIVE channel party c' (entanglement swap) if
   another membrane still holds a party of c' afterwards *)
let transferred_channel cfg' = function
  | Channeled (c', _) when not (Semantics.is_projective c') ->
      let holds = function
        | MoleculeMembrane ms ->
            List.exists
              (function ResourceMolecule (ChannelParty (c, _)) -> c = c' | _ -> false)
              ms
        | _ -> false
      in
      if List.exists holds cfg' then Some c' else None
  | _ -> None

let step_net ?(policy = AnyEdge) (net : net) (cfg : membrane list) :
    (nlabel * membrane list) list =
  Semantics.step ~keep_empty:true cfg
  |> List.filter_map (fun (l, cfg') ->
         match l with
         | Semantics.LChan (c, (i, j)) -> (
             let g = loc net i and h = loc net j in
             match rate net g h with
             | Some p when phi_edge policy net c (g, h) -> Some (NRate (p, g, h), cfg')
             | _ -> None (* rule Chan: edge missing or phi rejects *))
         | Semantics.LDecode (c, mu, (i, j)) ->
             let g = loc net i and h = loc net j in
             let chan' = Option.value (transferred_channel cfg' mu) ~default:c in
             if phi_nodes policy net chan' [ g; h ] then Some (NRate (1.0, g, h), cfg')
             else None (* rule Msg: success rate 1 *)
         | l -> Some (NPlain l, cfg') (* rule Trans *))

(* ---- likeliness traces and refinement (Defs. 16-19) ----------------------- *)

(* bounded likeliness trace set: sequences of p(g,h) labels (plain labels are
   invisible, as in Def. 16); small nets, so no state-memoization *)
let ntraces ?(policy = AnyEdge) ~depth (net : net) (cfg : membrane list) :
    nlabel list list =
  let acc = Hashtbl.create 64 in
  let render t = String.concat ";" (List.map string_of_nlabel t) in
  let rec go d cfg prefix =
    Hashtbl.replace acc (render (List.rev prefix)) (List.rev prefix);
    if d > 0 then
      List.iter
        (fun (nl, cfg') ->
          match nl with
          | NRate _ -> go (d - 1) cfg' (nl :: prefix)
          | NPlain _ -> go (d - 1) cfg' prefix)
        (step_net ~policy net cfg)
  in
  go depth cfg [];
  Hashtbl.fold (fun _ t l -> t :: l) acc [] |> List.sort compare

(* Def. 17: pointwise likeliness order — same endpoints, p <= p' *)
let rec likely_leq t1 t2 =
  match (t1, t2) with
  | [], [] -> true
  | NRate (p, g, h) :: a, NRate (p', g', h') :: b ->
      g = g' && h = h' && p <= p' +. 1e-9 && likely_leq a b
  | NPlain l :: a, NPlain l' :: b ->
      Semantics.string_of_label l = Semantics.string_of_label l' && likely_leq a b
  | _ -> false

(* Defs. 18-19: likeliness QAM trace refinement (bounded) *)
let likely_refines ?(pol1 = AnyEdge) ?(pol2 = AnyEdge) ~depth (net : net)
    (cfg : membrane list) : bool =
  let t2 = ntraces ~policy:pol2 ~depth net cfg in
  List.for_all
    (fun s1 -> List.exists (fun s2 -> likely_leq s1 s2) t2)
    (ntraces ~policy:pol1 ~depth net cfg)
