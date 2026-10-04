exception Parse_error

type loc = Lexing.position

let dummy_loc = Lexing.dummy_pos

type input = string

(* Legacy payload type used by the resource algebra (CombinedResource). *)
type message = Quantum of input | ClassicalData of input

(* Messages — the paper's mu (Fig. 4). The surface grammar produces [Named]
   only; the machine (Semantics) produces the rest. Projective channels use
   the "^c" naming convention inside [Channeled]. *)
type msg =
  | Blank                        (* the blank quantum resource ◦ *)
  | Named of string              (* a message / resource / channel name *)
  | Channeled of string * msg    (* delta.mu — channel holding content *)
  | Projected of string * msg    (* kept for compatibility; the machine uses
                                    Channeled with "^c" names *)
  | Residue of msg               (* †mu *)
  | Meet of msg * msg            (* mu1 ⊙ mu2 (Def. 5) *)

type resource =
  | SimpleResource of string     (* a named resource alpha (treated as alpha.◦) *)
  | NullResource                 (* o — the blank ◦ *)
  | ChannelParty of string * msg (* alpha.mu — this membrane's party of channel
                                    alpha holding mu (paper Fig. 5) *)
  | CombinedResource of resource * message (* legacy fragment *)
  | MeetOperation of resource * resource   (* legacy fragment (surface `&`) *)

type act =
  | NewChannel of string                         (* nu c.  — binds c *)
  | Send of { chan : string; arg : msg }         (* a!m.   — m is a message so
                                                    substituted residues travel *)
  | Receive of { chan : string; arg : string }   (* d?x.   — binds x *)
  | Encode of { chan : string; msg : msg }       (* c<-m.  — the paper's ◁ *)
  | Decode of { chan : string; binder : string } (* c->x.  — the paper's ▷, binds x *)

(* Every action carries its source position for typechecker diagnostics.
   [loc] is ignored by equivalence checking (see CheckEquivalence). *)
type action = { act : act; loc : loc }

let mk_action ?(loc = dummy_loc) act = { act; loc }

type process =
  | NullProcess                         (* 0 *)
  | ActionProcess of action * process   (* AR *)
  | Choice of process * process         (* R + T *)
  | Replication of process              (* !R *)
  | ProcessRef of string                (* named-definition reference; removed by elaboration *)

type molecule =
  | NullMolecule
  | ProcessMolecule of process
  | ResourceMolecule of resource

type membrane =
  | NullMembrane
  | MoleculeMembrane of molecule list         (* {|M|} *)
  | AirlockedMembrane of membrane * resource * membrane  (* P |[phi]| Q *)

type equivalence_result =
  | Equivalent
  | NotEquivalent of string

(* the channel an action acts on *)
let action_channel { act; _ } =
  match act with
  | NewChannel c -> c
  | Send { chan; _ } | Receive { chan; _ } | Encode { chan; _ } | Decode { chan; _ }
    -> chan

(* ---- names and binders --------------------------------------------------- *)

let rec msg_names = function
  | Blank -> []
  | Named n -> [ n ]
  | Channeled (c, m) | Projected (c, m) -> c :: msg_names m
  | Residue m -> msg_names m
  | Meet (a, b) -> msg_names a @ msg_names b

(* every name occurring in a process, free or bound *)
let rec names_of_process = function
  | NullProcess -> []
  | ProcessRef n -> [ n ]
  | ActionProcess ({ act; _ }, k) ->
      (match act with
      | NewChannel c -> [ c ]
      | Send { chan; arg } -> chan :: msg_names arg
      | Receive { chan; arg } -> [ chan; arg ]
      | Encode { chan; msg } -> chan :: msg_names msg
      | Decode { chan; binder } -> [ chan; binder ])
      @ names_of_process k
  | Choice (a, b) -> names_of_process a @ names_of_process b
  | Replication r -> names_of_process r

(* free names: names_of_process minus binder-scoped occurrences.
   nu c. binds c; d?x. binds x; c->x. binds x — each in its continuation. *)
let rec free_names = function
  | NullProcess -> []
  | ProcessRef n -> [ n ]
  | ActionProcess ({ act; _ }, k) -> (
      let fk = free_names k in
      match act with
      | NewChannel c -> List.filter (fun n -> n <> c) fk
      | Send { chan; arg } -> (chan :: msg_names arg) @ fk
      | Receive { chan; arg } -> chan :: List.filter (fun n -> n <> arg) fk
      | Encode { chan; msg } -> (chan :: msg_names msg) @ fk
      | Decode { chan; binder } -> chan :: List.filter (fun n -> n <> binder) fk)
  | Choice (a, b) -> free_names a @ free_names b
  | Replication r -> free_names r

(* a fresh name based on [base] avoiding every name in [used] *)
let freshen (base : string) (used : string list) : string =
  let rec go i =
    let cand = Printf.sprintf "%s_%d" base i in
    if List.mem cand used then go (i + 1) else cand
  in
  go 1

let rename_msg o n m =
  let r x = if x = o then n else x in
  let rec go = function
    | Blank -> Blank
    | Named x -> Named (r x)
    | Channeled (c, m) -> Channeled (r c, go m)
    | Projected (c, m) -> Projected (r c, go m)
    | Residue m -> Residue (go m)
    | Meet (a, b) -> Meet (go a, go b)
  in
  go m

(* rename every occurrence of [o] (free and binding) to [n]; capture-safe only
   when [n] is globally fresh — use with [freshen] *)
let rec rename_process (o : string) (n : string) (p : process) : process =
  let r x = if x = o then n else x in
  match p with
  | NullProcess -> NullProcess
  | ProcessRef x -> ProcessRef (r x)
  | ActionProcess ({ act; loc }, k) ->
      let act =
        match act with
        | NewChannel c -> NewChannel (r c)
        | Send { chan; arg } -> Send { chan = r chan; arg = rename_msg o n arg }
        | Receive { chan; arg } -> Receive { chan = r chan; arg = r arg }
        | Encode { chan; msg } -> Encode { chan = r chan; msg = rename_msg o n msg }
        | Decode { chan; binder } -> Decode { chan = r chan; binder = r binder }
      in
      ActionProcess ({ act; loc }, rename_process o n k)
  | Choice (a, b) -> Choice (rename_process o n a, rename_process o n b)
  | Replication p -> Replication (rename_process o n p)
