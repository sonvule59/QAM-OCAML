open Ast
open Interpreter

let rec check_resource_equivalence (r1 : resource) (r2 : resource) : bool =
  match r1, r2 with
  | SimpleResource r1, SimpleResource r2 -> check_resource_equivalence r1 r2
  | NullResource, NullResource -> true
  | CombinedResource (r11, m1), CombinedResource (r21, m2) -> check_resource_equivalence r11 r21 && m1 = m2
  | MeetOperation (r11, r12), MeetOperation (r21, r22) -> check_resource_equivalence r11 r21 && check_resource_equivalence r12 r22
  | _ -> false  

let rec check_process_equivalence (p1 : process) (p2 : process) : bool =
  match p1, p2 with
  | NullProcess, NullProcess -> true
  | ActionProcess (a1, p1), ActionProcess (a2, p2) -> a1 = a2 && check_process_equivalence p1 p2
  | Choice (p11, p12), Choice (p21, p22) -> check_process_equivalence p11 p21 && check_process_equivalence p12 p22
  | Replication p1, Replication p2 -> check_process_equivalence p1 p2
  | _ -> false

let rec check_molecule_equivalence (m1 : molecule) (m2 : molecule) : bool =
  match m1, m2 with
  | NullMolecule, NullMolecule -> true
  | ProcessMolecule p1, ProcessMolecule p2 -> check_process_equivalence p1 p2
  | ResourceMolecule r1, ResourceMolecule r2 -> check_resource_equivalence r1 r2
  | _ -> false

let test_membrane_equivalence (m1 : membrane) (m2 : membrane) : bool =
  match m1, m2 with
  | NullMembrane, NullMembrane -> true
  | MoleculeMembrane m1, MoleculeMembrane m2 -> check_molecule_equivalence m1 m2
  | AirlockedMembrane (m11, r1, m12), AirlockedMembrane (m21, r2, m22) -> check_membrane_equivalence m11 m21 && check_resource_equivalence r1 r2 && check_membrane_equivalence m12 m22
  | _ -> false



  