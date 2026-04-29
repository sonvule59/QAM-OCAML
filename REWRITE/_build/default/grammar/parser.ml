
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | RIGHTARROW
    | REP
    | RBRACE
    | QUESTION
    | PLUS
    | O
    | NU
    | LEFTARROW
    | LBRACE
    | IDENT of 
# 5 "grammar/parser.mly"
       (string)
# 24 "grammar/parser.ml"
  
    | EOF
    | DOT
    | COMMA
    | BANG
    | AIRLOCK_R
    | AIRLOCK_L
  
end

include MenhirBasics

# 1 "grammar/parser.mly"
  
  open Ast

# 41 "grammar/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : <empty>.
        Start symbol: main. *)

  | MenhirState02 : (('s, _menhir_box_main) _menhir_cell1_LBRACE, _menhir_box_main) _menhir_state
    (** State 02.
        Stack shape : LBRACE.
        Start symbol: main. *)

  | MenhirState03 : (('s, _menhir_box_main) _menhir_cell1_REP, _menhir_box_main) _menhir_state
    (** State 03.
        Stack shape : REP.
        Start symbol: main. *)

  | MenhirState23 : (('s, _menhir_box_main) _menhir_cell1_action, _menhir_box_main) _menhir_state
    (** State 23.
        Stack shape : action.
        Start symbol: main. *)

  | MenhirState25 : (('s, _menhir_box_main) _menhir_cell1_process, _menhir_box_main) _menhir_state
    (** State 25.
        Stack shape : process.
        Start symbol: main. *)

  | MenhirState35 : (('s, _menhir_box_main) _menhir_cell1_molecule, _menhir_box_main) _menhir_state
    (** State 35.
        Stack shape : molecule.
        Start symbol: main. *)

  | MenhirState37 : (('s, _menhir_box_main) _menhir_cell1_AIRLOCK_L, _menhir_box_main) _menhir_state
    (** State 37.
        Stack shape : AIRLOCK_L.
        Start symbol: main. *)

  | MenhirState39 : ((('s, _menhir_box_main) _menhir_cell1_AIRLOCK_L, _menhir_box_main) _menhir_cell1_membrane, _menhir_box_main) _menhir_state
    (** State 39.
        Stack shape : AIRLOCK_L membrane.
        Start symbol: main. *)

  | MenhirState42 : (((('s, _menhir_box_main) _menhir_cell1_AIRLOCK_L, _menhir_box_main) _menhir_cell1_membrane, _menhir_box_main) _menhir_cell1_resource, _menhir_box_main) _menhir_state
    (** State 42.
        Stack shape : AIRLOCK_L membrane resource.
        Start symbol: main. *)

  | MenhirState48 : (('s, _menhir_box_main) _menhir_cell1_membrane, _menhir_box_main) _menhir_state
    (** State 48.
        Stack shape : membrane.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_action = 
  | MenhirCell1_action of 's * ('s, 'r) _menhir_state * (Ast.action)

and ('s, 'r) _menhir_cell1_membrane = 
  | MenhirCell1_membrane of 's * ('s, 'r) _menhir_state * (Ast.membrane)

and ('s, 'r) _menhir_cell1_molecule = 
  | MenhirCell1_molecule of 's * ('s, 'r) _menhir_state * (Ast.molecule)

and ('s, 'r) _menhir_cell1_process = 
  | MenhirCell1_process of 's * ('s, 'r) _menhir_state * (Ast.process)

and ('s, 'r) _menhir_cell1_resource = 
  | MenhirCell1_resource of 's * ('s, 'r) _menhir_state * (Ast.resource)

and ('s, 'r) _menhir_cell1_AIRLOCK_L = 
  | MenhirCell1_AIRLOCK_L of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * 
# 5 "grammar/parser.mly"
       (string)
# 117 "grammar/parser.ml"


and ('s, 'r) _menhir_cell1_LBRACE = 
  | MenhirCell1_LBRACE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REP = 
  | MenhirCell1_REP of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (Ast.membrane list) [@@unboxed]

let _menhir_action_01 =
  fun _2 ->
    (
# 56 "grammar/parser.mly"
                 ( NewChannel _2 )
# 134 "grammar/parser.ml"
     : (Ast.action))

let _menhir_action_02 =
  fun _1 _3 ->
    (
# 57 "grammar/parser.mly"
                         ( Send (_1 ^ "!" ^ _3) )
# 142 "grammar/parser.ml"
     : (Ast.action))

let _menhir_action_03 =
  fun _1 _3 ->
    (
# 58 "grammar/parser.mly"
                             ( Receive (_1 ^ "?" ^ _3) )
# 150 "grammar/parser.ml"
     : (Ast.action))

let _menhir_action_04 =
  fun _1 _3 ->
    (
# 59 "grammar/parser.mly"
                              ( LeftCombine (_1 ^ "<-" ^ _3) )
# 158 "grammar/parser.ml"
     : (Ast.action))

let _menhir_action_05 =
  fun _1 _3 ->
    (
# 60 "grammar/parser.mly"
                               ( RightCombine (_1 ^ "->" ^ _3) )
# 166 "grammar/parser.ml"
     : (Ast.action))

let _menhir_action_06 =
  fun _1 _3 ->
    (
# 48 "grammar/parser.mly"
                       ( ActionProcess (_1, _3) )
# 174 "grammar/parser.ml"
     : (Ast.process))

let _menhir_action_07 =
  fun _1 ->
    (
# 49 "grammar/parser.mly"
           ( ActionProcess (_1, NullProcess) )
# 182 "grammar/parser.ml"
     : (Ast.process))

let _menhir_action_08 =
  fun _1 ->
    (
# 20 "grammar/parser.mly"
                  ( _1 )
# 190 "grammar/parser.ml"
     : (Ast.membrane list))

let _menhir_action_09 =
  fun () ->
    let _ = (
# 21 "grammar/parser.mly"
          ( raise Parse_error )
# 198 "grammar/parser.ml"
     : (Ast.membrane list)) in
    prerr_string "Menhir: misuse: the semantic action associated with the production\nmain -> error\nis expected to abort the parser, but does not do so.\n";
    assert false

let _menhir_action_10 =
  fun _2 ->
    (
# 28 "grammar/parser.mly"
                            ( MoleculeMembrane _2 )
# 208 "grammar/parser.ml"
     : (Ast.membrane))

let _menhir_action_11 =
  fun _2 _4 _6 ->
    (
# 29 "grammar/parser.mly"
                                                               ( AirlockedMembrane (_2, _4, _6) )
# 216 "grammar/parser.ml"
     : (Ast.membrane))

let _menhir_action_12 =
  fun _1 ->
    (
# 24 "grammar/parser.mly"
             ( [_1] )
# 224 "grammar/parser.ml"
     : (Ast.membrane list))

let _menhir_action_13 =
  fun _1 _3 ->
    (
# 25 "grammar/parser.mly"
                             ( _1 :: _3 )
# 232 "grammar/parser.ml"
     : (Ast.membrane list))

let _menhir_action_14 =
  fun _1 ->
    (
# 36 "grammar/parser.mly"
            ( ProcessMolecule _1 )
# 240 "grammar/parser.ml"
     : (Ast.molecule))

let _menhir_action_15 =
  fun _1 ->
    (
# 37 "grammar/parser.mly"
             ( ResourceMolecule _1 )
# 248 "grammar/parser.ml"
     : (Ast.molecule))

let _menhir_action_16 =
  fun _1 ->
    (
# 32 "grammar/parser.mly"
             ( [_1] )
# 256 "grammar/parser.ml"
     : (Ast.molecule list))

let _menhir_action_17 =
  fun _1 _3 ->
    (
# 33 "grammar/parser.mly"
                             ( _1 :: _3 )
# 264 "grammar/parser.ml"
     : (Ast.molecule list))

let _menhir_action_18 =
  fun _2 ->
    (
# 44 "grammar/parser.mly"
                       ( Replication _2 )
# 272 "grammar/parser.ml"
     : (Ast.process))

let _menhir_action_19 =
  fun _1 ->
    (
# 45 "grammar/parser.mly"
                   ( _1 )
# 280 "grammar/parser.ml"
     : (Ast.process))

let _menhir_action_20 =
  fun _1 _3 ->
    (
# 40 "grammar/parser.mly"
                                ( Choice (_1, _3) )
# 288 "grammar/parser.ml"
     : (Ast.process))

let _menhir_action_21 =
  fun _1 ->
    (
# 41 "grammar/parser.mly"
                   ( _1 )
# 296 "grammar/parser.ml"
     : (Ast.process))

let _menhir_action_22 =
  fun _1 ->
    (
# 52 "grammar/parser.mly"
          ( SimpleResource _1 )
# 304 "grammar/parser.ml"
     : (Ast.resource))

let _menhir_action_23 =
  fun () ->
    (
# 53 "grammar/parser.mly"
      ( NullResource )
# 312 "grammar/parser.ml"
     : (Ast.resource))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | RIGHTARROW ->
        "RIGHTARROW"
    | REP ->
        "REP"
    | RBRACE ->
        "RBRACE"
    | QUESTION ->
        "QUESTION"
    | PLUS ->
        "PLUS"
    | O ->
        "O"
    | NU ->
        "NU"
    | LEFTARROW ->
        "LEFTARROW"
    | LBRACE ->
        "LBRACE"
    | IDENT _ ->
        "IDENT"
    | EOF ->
        "EOF"
    | DOT ->
        "DOT"
    | COMMA ->
        "COMMA"
    | BANG ->
        "BANG"
    | AIRLOCK_R ->
        "AIRLOCK_R"
    | AIRLOCK_L ->
        "AIRLOCK_L"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_45 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let _1 = _v in
      let _v = _menhir_action_08 _1 in
      MenhirBox_main _v
  
  let rec _menhir_goto_membranes : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_45 _menhir_stack _v
      | MenhirState48 ->
          _menhir_run_49 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_49 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_membrane -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let MenhirCell1_membrane (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_13 _1 _3 in
      _menhir_goto_membranes _menhir_stack _v _menhir_s
  
  let rec _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState02 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REP ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | O ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NU ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REP (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REP ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NU ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _2 = _v in
              let _v = _menhir_action_01 _2 in
              _menhir_goto_action _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_action : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_stack = MenhirCell1_action (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState23 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REP ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NU ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COMMA | PLUS | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_goto_atomic_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEFTARROW ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BANG ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
              let _3 = _v in
              let _v = _menhir_action_05 _1 _3 in
              _menhir_goto_action _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
              let _3 = _v in
              let _v = _menhir_action_03 _1 _3 in
              _menhir_goto_action _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
              let _3 = _v in
              let _v = _menhir_action_04 _1 _3 in
              _menhir_goto_action _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_17 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
              let _3 = _v in
              let _v = _menhir_action_02 _1 _3 in
              _menhir_goto_action _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_atomic_process : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_19 _1 in
      _menhir_goto_prefix_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_prefix_process : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState03 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState02 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState23 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState35 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_REP -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_REP (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_18 _2 in
      _menhir_goto_prefix_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_26 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_process -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_process (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_20 _1 _3 in
      _menhir_goto_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_process : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState23 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState35 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_24 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_action as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_process (_menhir_stack, _menhir_s, _v) in
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RBRACE ->
          let MenhirCell1_action (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_06 _1 _3 in
          _menhir_goto_atomic_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_25 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_process -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState25 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REP ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NU ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_31 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_process (_menhir_stack, _menhir_s, _v) in
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_14 _1 in
          _menhir_goto_molecule _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_molecule : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_molecule (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState35 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REP ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | O ->
              _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NU ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_16 _1 in
          _menhir_goto_molecules _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_28 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_23 () in
      _menhir_goto_resource _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_resource : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState02 ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState35 ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState39 ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_30 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_15 _1 in
      _menhir_goto_molecule _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_41 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_AIRLOCK_L, _menhir_box_main) _menhir_cell1_membrane as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_resource (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState42 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AIRLOCK_L ->
              _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_37 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_AIRLOCK_L (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState37 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AIRLOCK_L ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_29 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEFTARROW ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BANG ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_22 _1 in
          _menhir_goto_resource _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_molecules : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState02 ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState35 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_32 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LBRACE -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LBRACE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_10 _2 in
      _menhir_goto_membrane _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_membrane : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState37 ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState42 ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState48 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_38 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_AIRLOCK_L as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_membrane (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | O ->
              _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState39
          | IDENT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_1 =
                let _1 = _v_0 in
                _menhir_action_22 _1
              in
              _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState39 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_43 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_AIRLOCK_L, _menhir_box_main) _menhir_cell1_membrane, _menhir_box_main) _menhir_cell1_resource -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | AIRLOCK_R ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_resource (_menhir_stack, _, _4) = _menhir_stack in
          let MenhirCell1_membrane (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_AIRLOCK_L (_menhir_stack, _menhir_s) = _menhir_stack in
          let _6 = _v in
          let _v = _menhir_action_11 _2 _4 _6 in
          _menhir_goto_membrane _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_47 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_membrane (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState48 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AIRLOCK_L ->
              _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_12 _1 in
          _menhir_goto_membranes _menhir_stack _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_36 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_molecule -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_molecule (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_17 _1 _3 in
      _menhir_goto_molecules _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_27 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_21 _1 in
      _menhir_goto_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | AIRLOCK_L ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | _ ->
          let _v = _menhir_action_09 () in
          MenhirBox_main _v
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
