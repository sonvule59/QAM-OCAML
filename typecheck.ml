open Ast

(* ============================================================================
   Static checker for QAM configurations (Phase 1 of the roadmap).

   Implements, for the supported fragment:
   - Channel-kind inference (the paper's Omega, Fig. 4): a channel is QUANTUM
     if it is nu-created, encoded (<-) or decoded (->); CLASSICAL if it is
     !-sent. Receive (?) is polymorphic (the paper's delta). Conflicts are
     type errors.
   - Well-formedness (paper Def. 23): every nu-created channel has exactly two
     creating membranes, and each creating membrane holds enough blank `o`
     resources (the Cohere rule consumes one per party).
   - Linearity = static no-cloning (paper Thm. 3): a named quantum resource is
     consumed by at most one encode; a channel party is used as a quantum
     message (entanglement swap) at most once per membrane; a channel is
     decoded at most once.

   Binders are respected: receive binds its variable to (an alias of) the
   received channel; decode binds its variable to a classical residue. Using a
   residue as a channel is a type error.

   All errors carry the source position of the offending action.
   ========================================================================== *)

type kind = KQuantum | KClassical

type error = { eloc : loc option; emsg : string }

let string_of_error { eloc; emsg } =
  match eloc with
  | Some p when p.Lexing.pos_lnum > 0 ->
      Printf.sprintf "line %d, column %d: %s" p.Lexing.pos_lnum
        (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1)
        emsg
  | _ -> emsg

(* what a bound variable stands for *)
type binding = Alias of string (* receive binder: the channel it aliases *)
             | ResidueVar      (* decode binder: a classical residue *)

type ctx = {
  errors : error list ref;
  (* global channel-kind constraints: name -> (kind, loc of first constraint) *)
  kinds : (string, kind * loc) Hashtbl.t;
  (* channel -> creating membranes (idx, loc), in encounter order *)
  creators : (string, (int * loc) list) Hashtbl.t;
  (* channel -> decode count *)
  decodes : (string, int * loc) Hashtbl.t;
  (* (membrane, resource/channel name) -> quantum-consumption count *)
  consumed : (int * string, int * loc) Hashtbl.t;
  (* channel -> quantum-payload (encode of a quantum message) count. The paper:
     "the created quantum channel can only be used to teleport one message" *)
  payloads : (string, int * loc) Hashtbl.t;
  (* encodes whose message is a receive-bound alias: whether that is a quantum
     channel party (QES) or classical data depends on the aliased channel's
     kind, which is only known after constraint solving — so they are recorded
     here as (target, aliased channel, membrane, loc) and resolved at the end *)
  pending : (string * string * int * loc) list ref;
}

let err ctx ?loc fmt =
  Printf.ksprintf (fun m -> ctx.errors := { eloc = loc; emsg = m } :: !(ctx.errors)) fmt

let constrain ctx name k loc =
  match Hashtbl.find_opt ctx.kinds name with
  | None -> Hashtbl.replace ctx.kinds name (k, loc)
  | Some (k0, l0) when k0 <> k ->
      let show = function KQuantum -> "quantum" | KClassical -> "classical" in
      err ctx ~loc "channel %s used as %s here but as %s at line %d" name (show k)
        (show k0) l0.Lexing.pos_lnum
  | Some _ -> ()

(* resolve a name through receive-aliases; Error if it goes through a residue *)
let resolve env name =
  let rec go seen n =
    match List.assoc_opt n env with
    | Some (Alias c) when not (List.mem n seen) -> go (n :: seen) c
    | Some (Alias _) -> Ok n
    | Some ResidueVar -> Error n
    | None -> Ok n
  in
  go [] name

let bump tbl key loc =
  match Hashtbl.find_opt tbl key with
  | None -> Hashtbl.replace tbl key (1, loc)
  | Some (n, _) -> Hashtbl.replace tbl key (n + 1, loc)

let membrane_named_resources = function
  | MoleculeMembrane ms ->
      List.filter_map (function ResourceMolecule (SimpleResource n) -> Some n | _ -> None) ms
  | _ -> []

let membrane_blanks = function
  | MoleculeMembrane ms ->
      List.length (List.filter (function ResourceMolecule NullResource -> true | _ -> false) ms)
  | _ -> 0

(* walk one process in membrane [g]; [env] binds local variables *)
let rec walk ctx g resources env p =
  match p with
  | NullProcess -> ()
  | ProcessRef n -> err ctx "undefined process reference %s (no matching `let %s = ... ;`)" n n
  | Choice (a, b) ->
      walk ctx g resources env a;
      walk ctx g resources env b
  | Replication r -> walk ctx g resources env r
  | ActionProcess ({ act; loc }, k) -> (
      let chan_of name =
        match resolve env name with
        | Ok c -> Some c
        | Error v ->
            err ctx ~loc "%s is a classical residue and cannot be used as a channel" v;
            None
      in
      match act with
      | NewChannel c ->
          constrain ctx c KQuantum loc;
          (match Hashtbl.find_opt ctx.creators c with
          | None -> Hashtbl.replace ctx.creators c [ (g, loc) ]
          | Some l -> Hashtbl.replace ctx.creators c (l @ [ (g, loc) ]));
          walk ctx g resources env k
      | Send { chan; _ } ->
          (match chan_of chan with
          | Some c -> constrain ctx c KClassical loc
          | None -> ());
          walk ctx g resources env k
      | Receive { chan; arg } ->
          (* polymorphic: no kind constraint; binder aliases the channel *)
          (match chan_of chan with
          | Some c -> walk ctx g resources ((arg, Alias c) :: env) k
          | None -> walk ctx g resources env k)
      | Decode { chan; binder } ->
          (match chan_of chan with
          | Some c ->
              constrain ctx c KQuantum loc;
              bump ctx.decodes c loc
          | None -> ());
          walk ctx g resources ((binder, ResidueVar) :: env) k
      | Encode { chan; msg } ->
          (match chan_of chan with
          | Some c -> constrain ctx c KQuantum loc
          | None -> ());
          (let target = match resolve env chan with Ok c -> Some c | Error _ -> None in
           let quantum_payload () =
             match target with
             | Some c -> bump ctx.payloads c loc
             | None -> ()
           in
           match msg with
          | Named n -> (
              match List.assoc_opt n env with
              | Some ResidueVar -> () (* classical residue: freely usable *)
              | Some (Alias c) -> (
                  (* quantum party (QES) or classical data? resolved after
                     kind solving *)
                  match target with
                  | Some t -> ctx.pending := (t, c, g, loc) :: !(ctx.pending)
                  | None -> ())
              | None ->
                  if List.mem n resources then begin
                    (* named quantum resource in this membrane: linear *)
                    bump ctx.consumed (g, n) loc;
                    quantum_payload ()
                  end
                  else if Hashtbl.mem ctx.creators n then begin
                    (* this membrane's party of a channel, sent as a message *)
                    bump ctx.consumed (g, n) loc;
                    quantum_payload ()
                  end
                  (* otherwise: a classical input bit — no linearity constraint *))
          | Blank | Channeled _ | Projected _ | Residue _ | Meet _ ->
              err ctx ~loc "structured encode messages are not supported yet");
          walk ctx g resources env k)

let check (membranes : membrane list) :
    ((string, kind) Hashtbl.t, error list) result =
  let ctx =
    {
      errors = ref [];
      kinds = Hashtbl.create 16;
      creators = Hashtbl.create 16;
      decodes = Hashtbl.create 16;
      consumed = Hashtbl.create 16;
      payloads = Hashtbl.create 8;
      pending = ref [];
    }
  in
  (* pre-register creators so channel-party messages resolve regardless of
     membrane order *)
  List.iteri
    (fun _g m ->
      match m with
      | MoleculeMembrane ms ->
          List.iter
            (function
              | ProcessMolecule p ->
                  List.iter
                    (fun c ->
                      if not (Hashtbl.mem ctx.creators c) then
                        Hashtbl.replace ctx.creators c [])
                    (Compile.created_channels p)
              | _ -> ())
            ms
      | _ -> ())
    membranes;
  (* clear and re-collect creators with locations during the walk *)
  let precreated = Hashtbl.copy ctx.creators in
  Hashtbl.reset ctx.creators;
  Hashtbl.iter (fun c _ -> Hashtbl.replace ctx.creators c []) precreated;
  List.iteri
    (fun g m ->
      let resources = membrane_named_resources m in
      match m with
      | MoleculeMembrane ms ->
          List.iter
            (function ProcessMolecule p -> walk ctx g resources [] p | _ -> ())
            ms
      | _ -> ())
    membranes;
  (* well-formedness: exactly two creating membranes per channel *)
  Hashtbl.iter
    (fun c creators ->
      let mems = List.sort_uniq compare (List.map fst creators) in
      match (mems, creators) with
      | [], _ -> ()
      | [ _ ], (_, loc) :: _ ->
          err ctx ~loc "channel %s is created by only one membrane; Cohere needs a partner" c
      | _ :: _ :: _ :: _, (_, loc) :: _ ->
          err ctx ~loc "channel %s is created by %d membranes; a quantum channel has exactly two parties" c
            (List.length mems)
      | _ -> ())
    ctx.creators;
  (* blank accounting: each creating membrane consumes one blank per channel *)
  List.iteri
    (fun g m ->
      let needed =
        Hashtbl.fold
          (fun _c creators acc ->
            if List.exists (fun (g', _) -> g' = g) creators then acc + 1 else acc)
          ctx.creators 0
      in
      let have = membrane_blanks m in
      if needed > have then
        err ctx "membrane %d creates %d channel(s) but holds only %d blank 'o' resource(s)" g
          needed have)
    membranes;
  (* resolve pending alias-encodes: the message is a quantum channel party
     (counts for linearity and payload) only if the aliased channel is quantum *)
  List.iter
    (fun (target, aliased, g, loc) ->
      match Hashtbl.find_opt ctx.kinds aliased with
      | Some (KQuantum, _) ->
          bump ctx.consumed (g, aliased) loc;
          bump ctx.payloads target loc
      | _ -> () (* classical data: no linearity constraint *))
    !(ctx.pending);
  (* linearity: decode-once per channel *)
  Hashtbl.iter
    (fun c (n, loc) ->
      if n > 1 then err ctx ~loc "channel %s is decoded %d times; a channel can be decoded once" c n)
    ctx.decodes;
  (* one quantum payload per channel (the paper: a quantum channel can only be
     used to teleport one message) *)
  Hashtbl.iter
    (fun c (n, loc) ->
      if n > 1 then
        err ctx ~loc
          "channel %s is encoded with %d quantum payloads; a quantum channel teleports one message"
          c n)
    ctx.payloads;
  (* linearity: quantum resources / channel parties consumed at most once *)
  Hashtbl.iter
    (fun (g, n) (count, loc) ->
      if count > 1 then
        err ctx ~loc
          "no-cloning violation: quantum resource %s in membrane %d is consumed %d times" n g
          count)
    ctx.consumed;
  if !(ctx.errors) = [] then
    Ok (Hashtbl.fold (fun n (k, _) acc -> Hashtbl.replace acc n k; acc) ctx.kinds
          (Hashtbl.create 16))
  else Error (List.rev !(ctx.errors))
