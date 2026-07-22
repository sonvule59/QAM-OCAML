open Ast

let rec check_resource_equivalence (r1 : resource) (r2 : resource) : bool =
  match r1, r2 with
  | SimpleResource s1, SimpleResource s2 -> s1 = s2
  | NullResource, NullResource -> true
  | CombinedResource (r11, m1), CombinedResource (r21, m2) ->
    check_resource_equivalence r11 r21 && m1 = m2
  | MeetOperation (r11, r12), MeetOperation (r21, r22) ->
    check_resource_equivalence r11 r21 && check_resource_equivalence r12 r22
  | _ -> false

let rec check_process_equivalence (p1 : process) (p2 : process) : bool =
  match p1, p2 with
  | NullProcess, NullProcess -> true
  | ActionProcess (a1, p1), ActionProcess (a2, p2) ->
    a1 = a2 && check_process_equivalence p1 p2
  | Choice (p11, p12), Choice (p21, p22) ->
    check_process_equivalence p11 p21 && check_process_equivalence p12 p22
  | Replication p1, Replication p2 -> check_process_equivalence p1 p2
  | _ -> false

let check_molecule_equivalence (m1 : molecule) (m2 : molecule) : bool =
  match m1, m2 with
  | NullMolecule, NullMolecule -> true
  | ProcessMolecule p1, ProcessMolecule p2 -> check_process_equivalence p1 p2
  | ResourceMolecule r1, ResourceMolecule r2 -> check_resource_equivalence r1 r2
  | _ -> false

let rec check_membrane_equivalence (m1 : membrane) (m2 : membrane) : bool =
  match m1, m2 with
  | NullMembrane, NullMembrane -> true
  | MoleculeMembrane ml1, MoleculeMembrane ml2 ->
    List.length ml1 = List.length ml2 &&
    List.for_all2 check_molecule_equivalence ml1 ml2
  | AirlockedMembrane (m11, r1, m12), AirlockedMembrane (m21, r2, m22) ->
    check_membrane_equivalence m11 m21 &&
    check_resource_equivalence r1 r2 &&
    check_membrane_equivalence m12 m22
  | _ -> false

(* ---- Phase 2: canonicalizer for the supported reducing fragment ----

   [canonicalize] rewrites a (already-normalized) term into a canonical shape so
   that structurally-different-but-equivalent terms compare equal. It implements
   exactly two laws, plus a trivial cleanup:

     A. Soup-as-multiset : molecule order in a MoleculeMembrane is irrelevant, so
        sort by the total order [Stdlib.compare] (on canonicalized molecules).
     B. Meet commutativity: (a & b) and (b & a) canonicalize identically.
     C. Drop NullMolecule (it never arises from parsing/reduction, so this does
        not affect Phase 1 golden normal forms, which use ProcessMolecule
        NullProcess i.e. the printed "0", not NullMolecule).

   OUT OF SCOPE (see README "Equivalence policy"): meet associativity flattening,
   Choice commutativity, alpha-equivalence, bisimulation. *)
let rec canon_resource (r : resource) : resource =
  match r with
  | SimpleResource _ | NullResource -> r
  | CombinedResource (r1, m) -> CombinedResource (canon_resource r1, m)
  | MeetOperation (r1, r2) ->
    let a = canon_resource r1 and b = canon_resource r2 in
    if compare a b <= 0 then MeetOperation (a, b) else MeetOperation (b, a)

let canon_molecule (mol : molecule) : molecule =
  match mol with
  | ResourceMolecule r -> ResourceMolecule (canon_resource r)
  | ProcessMolecule _ | NullMolecule -> mol

let rec canonicalize (m : membrane) : membrane =
  match m with
  | NullMembrane -> NullMembrane
  | MoleculeMembrane ms ->
    let ms =
      ms |> List.map canon_molecule |> List.filter (fun x -> x <> NullMolecule)
    in
    MoleculeMembrane (List.sort compare ms)
  | AirlockedMembrane (l, r, rt) ->
    AirlockedMembrane (canonicalize l, canon_resource r, canonicalize rt)

(* Equivalence = structural walk over canonicalized terms. *)
let equivalent (m1 : membrane) (m2 : membrane) : bool =
  check_membrane_equivalence (canonicalize m1) (canonicalize m2)
