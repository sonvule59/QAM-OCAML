open Ast

(* ============================================================================
   Front-end driver and term printers.

   This module holds parsing (with positioned errors), `let`-definition
   elaboration, nu-binder freshening, and the surface-syntax printers. The
   REDUCTION semantics lives in semantics.ml (the paper's configuration-level
   LTS); the legacy single-membrane reducer that used to live here was retired
   once the machine covered its fragment.
   ========================================================================== *)

type parse_error = Lex_err of string * string | Parse_err of string
(* second component: "line L, column C" of the failure *)

let position_string (lb : Lexing.lexbuf) : string =
  let p = lb.Lexing.lex_curr_p in
  Printf.sprintf "line %d, column %d" p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1)

(* ---- named-definition elaboration ----------------------------------------
   `let A = process ;` definitions are substituted away right after parsing:
   ProcessRef nodes (and bare molecule names that match a definition) are
   replaced by their bodies. A depth guard keeps self-recursive definitions
   from looping; any ProcessRef surviving elaboration is flagged by the
   typechecker as undefined. *)

let rec subst_refs (defs : (string * process) list) (depth : int) (p : process) :
    process =
  if depth <= 0 then p
  else
    match p with
    | ProcessRef n -> (
        match List.assoc_opt n defs with
        | Some body -> subst_refs defs (depth - 1) body
        | None -> p)
    | ActionProcess (a, k) -> ActionProcess (a, subst_refs defs depth k)
    | Choice (a, b) -> Choice (subst_refs defs depth a, subst_refs defs depth b)
    | Replication r -> Replication (subst_refs defs depth r)
    | NullProcess -> NullProcess

let elaborate (defs : (string * process) list) (membranes : membrane list) :
    membrane list =
  let elab_molecule = function
    | ResourceMolecule (SimpleResource n) when List.mem_assoc n defs ->
        (* a bare name in molecule position parses as a resource; promote it to
           the defined process (documented resolution rule) *)
        ProcessMolecule (subst_refs defs 32 (ProcessRef n))
    | ProcessMolecule p -> ProcessMolecule (subst_refs defs 32 p)
    | m -> m
  in
  let rec elab_membrane = function
    | MoleculeMembrane ms -> MoleculeMembrane (List.map elab_molecule ms)
    | AirlockedMembrane (l, r, rt) ->
        AirlockedMembrane (elab_membrane l, r, elab_membrane rt)
    | NullMembrane -> NullMembrane
  in
  List.map elab_membrane membranes

let parse_membranes_from_lexbuf (lb : Lexing.lexbuf) :
    (membrane list, parse_error) result =
  try
    let defs, membranes = Parser.main Lexer.token lb in
    Ok (elaborate defs membranes)
  with
  | Lexer.SyntaxError msg -> Error (Lex_err (msg, position_string lb))
  | Parser.Error -> Error (Parse_err (position_string lb))
  | Ast.Parse_error -> Error (Parse_err (position_string lb))

let parse_membranes_from_string (input : string) :
    (membrane list, parse_error) result =
  parse_membranes_from_lexbuf (Lexing.from_string input)

let print_parse_error = function
  | Lex_err (msg, pos) -> Printf.printf "Lexer error at %s: %s\n%!" pos msg
  | Parse_err pos -> Printf.printf "Parse error at %s.\n%!" pos

(* Rename each nu-bound channel in [p] to a name fresh w.r.t. [used]. Used by
   the machine at replication unfolding so independent copies of `nu c.` get
   distinct channels instead of colliding by name. *)
let freshen_nus (used : string list) (p : process) : process =
  let rec go used p =
    match p with
    | ActionProcess (({ act = NewChannel c; loc } as _a), k) ->
        let c', k' =
          if List.mem c used then
            let c' = freshen c used in
            (c', rename_process c c' k)
          else (c, k)
        in
        ActionProcess ({ act = NewChannel c'; loc }, go (c' :: used) k')
    | ActionProcess (a, k) -> ActionProcess (a, go used k)
    | Choice (a, b) -> Choice (go used a, go used b)
    | Replication r -> Replication (go used r)
    | (NullProcess | ProcessRef _) as x -> x
  in
  go used p

(* ---- printers (surface syntax) -------------------------------------------- *)

let rec string_of_msg = function
  | Blank -> "o"
  | Named n -> n
  | Channeled (c, m) -> c ^ "." ^ string_of_msg m
  | Projected (c, m) -> "^" ^ c ^ "." ^ string_of_msg m
  | Residue m -> "~" ^ string_of_msg m
  | Meet (a, b) -> "(" ^ string_of_msg a ^ " & " ^ string_of_msg b ^ ")"

let string_of_action { act; _ } =
  match act with
  | NewChannel c -> "nu " ^ c ^ "."
  | Send { chan; arg } -> chan ^ "!" ^ string_of_msg arg ^ "."
  | Receive { chan; arg } -> chan ^ "?" ^ arg ^ "."
  | Encode { chan; msg } -> chan ^ "<-" ^ string_of_msg msg ^ "."
  | Decode { chan; binder } -> chan ^ "->" ^ binder ^ "."

let rec string_of_process = function
  | NullProcess -> "0"
  | ProcessRef n -> n
  | ActionProcess (a, NullProcess) -> string_of_action a
  | ActionProcess (a, p) -> string_of_action a ^ string_of_process p
  | Choice (p, q) -> string_of_process p ^ " + " ^ string_of_process q
  | Replication p -> "repl " ^ string_of_process p

let rec string_of_resource = function
  | SimpleResource s -> s
  | NullResource -> "o"
  | ChannelParty (c, m) -> c ^ "." ^ string_of_msg m
  | CombinedResource (r, m) ->
      string_of_resource r ^ ".("
      ^ (match m with Quantum s -> "q:" ^ s | ClassicalData s -> "c:" ^ s)
      ^ ")"
  | MeetOperation (r1, r2) -> string_of_resource r1 ^ " & " ^ string_of_resource r2

let string_of_molecule = function
  | NullMolecule -> "0"
  | ProcessMolecule p -> string_of_process p
  | ResourceMolecule r -> string_of_resource r

let rec string_of_membrane = function
  | NullMembrane -> "{}"
  | MoleculeMembrane ms ->
      "{ " ^ String.concat ", " (List.map string_of_molecule ms) ^ " }"
  | AirlockedMembrane (l, r, rt) ->
      "|[ " ^ string_of_membrane l ^ ", " ^ string_of_resource r ^ ", "
      ^ string_of_membrane rt ^ " ]|"
