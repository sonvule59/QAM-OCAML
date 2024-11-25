type input = string
type message = Quantum of input | ClassicalData of input

type resource =
  | SimpleResource of resource   (* alpha.mu *)
  | NullResource               (* o *)
  | CombinedResource of resource * message (* alpha.(mu1 o mu2) *)
  | MeetOperation of resource * resource  (* mu1 o mu2 *)

type action =
  | NewChannel of string                (* nu c. *)
  | Send of string                      (* a!i. *)
  | Receive of string                   (* delta?(x). *)
  | LeftCombine of string               (* alpha <- k. encoding *)
  | RightCombine of string              (* c -> (x). decoding *)

type process =
  | NullProcess                         (* 0 *)
  | ActionProcess of action * process   (* AR *)
  | Choice of process * process         (* R + T *)
  | Replication of process              (* !R *)

type molecule =
  | NullMolecule
  | ProcessMolecule of process
  | ResourceMolecule of resource
  (* | MoleculeMembrane of molecule list         {|M|} *)

type membrane =
  | NullMembrane
  | MoleculeMembrane of molecule list         (* {|M|} *)
  | AirlockedMembrane of membrane * resource * membrane  (* P |[phi]| Q *)

(* New type for equivalence checking results *)
type equivalence_result =
  | Equivalent
  | NotEquivalent of string

  
