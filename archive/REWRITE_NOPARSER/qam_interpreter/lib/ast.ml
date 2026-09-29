module Ast = struct

  type input = string

  type message =
    | Quantum of input
    | ClassicalData of input

  type resource =
    | SimpleResource of resource  (* Basic resource *)
    | NullResource               (* Empty resource *)
    | CombinedResource of resource * message (* Composite resource *)
    | MeetOperation of resource * resource  (* Meet operator for combining resources *)

  type action =
    | NewChannel of string      (* Create a new channel *)
    | Send of string            (* Send a message over a channel *)
    | Receive of string         (* Receive a message from a channel *)
    | Encode of string          (* Encode a message into a channel *)
    | Decode of string          (* Decode a message from a channel *)

  type process =
    | NullProcess                       (* Represents the null process *)
    | ActionProcess of action * process (* Sequential action *)
    | Choice of process * process       (* Choice between two processes *)
    | Replication of process            (* Replication of a process *)

  type molecule =
    | NullMolecule                    (* Represents an empty molecule *)
    | ProcessMolecule of process      (* Molecule containing a process *)
    | ResourceMolecule of resource    (* Molecule containing a resource *)

  type membrane =
    | NullMembrane                    (* Represents an empty membrane *)
    | MoleculeMembrane of molecule list (* Membrane containing molecules *)
    | AirlockedMembrane of molecule list * resource * process
        (* Represents a membrane with restricted access *)
end