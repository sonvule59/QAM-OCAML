
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | RIGHTARROW
    | RBRACE
    | QUESTION
    | O
    | NU
    | LEFTARROW
    | LBRACE
    | IDENT of (
# 5 "parser.mly"
       (string)
# 22 "parser.ml"
  )
    | EOF
    | DOT
    | COMMA
    | BANG
  
end

include MenhirBasics

# 1 "parser.mly"
  
open Ast
exception ParseError 

# 38 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState02 : (('s, _menhir_box_main) _menhir_cell1_LBRACE, _menhir_box_main) _menhir_state
    (** State 02.
        Stack shape : LBRACE.
        Start symbol: main. *)

  | MenhirState25 : (('s, _menhir_box_main) _menhir_cell1_molecule, _menhir_box_main) _menhir_state
    (** State 25.
        Stack shape : molecule.
        Start symbol: main. *)

  | MenhirState28 : (('s, _menhir_box_main) _menhir_cell1_action, _menhir_box_main) _menhir_state
    (** State 28.
        Stack shape : action.
        Start symbol: main. *)

  | MenhirState33 : (('s, _menhir_box_main) _menhir_cell1_membrane, _menhir_box_main) _menhir_state
    (** State 33.
        Stack shape : membrane.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_action = 
  | MenhirCell1_action of 's * ('s, 'r) _menhir_state * (
# 11 "parser.mly"
      (Ast.action)
# 71 "parser.ml"
)

and ('s, 'r) _menhir_cell1_membrane = 
  | MenhirCell1_membrane of 's * ('s, 'r) _menhir_state * (
# 12 "parser.mly"
      (Ast.membrane)
# 78 "parser.ml"
)

and ('s, 'r) _menhir_cell1_molecule = 
  | MenhirCell1_molecule of 's * ('s, 'r) _menhir_state * (
# 14 "parser.mly"
      (Ast.molecule)
# 85 "parser.ml"
)

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * (
# 5 "parser.mly"
       (string)
# 92 "parser.ml"
)

and ('s, 'r) _menhir_cell1_LBRACE = 
  | MenhirCell1_LBRACE of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (
# 10 "parser.mly"
      (Ast.membrane list)
# 102 "parser.ml"
) [@@unboxed]

let _menhir_action_01 =
  fun _2 ->
    (
# 48 "parser.mly"
                 ( NewChannel _2 )
# 110 "parser.ml"
     : (
# 11 "parser.mly"
      (Ast.action)
# 114 "parser.ml"
    ))

let _menhir_action_02 =
  fun _1 _3 ->
    (
# 49 "parser.mly"
                         ( Send (_1 ^ "!" ^ _3) )
# 122 "parser.ml"
     : (
# 11 "parser.mly"
      (Ast.action)
# 126 "parser.ml"
    ))

let _menhir_action_03 =
  fun _1 _3 ->
    (
# 50 "parser.mly"
                             ( Receive (_1 ^ "?" ^ _3) )
# 134 "parser.ml"
     : (
# 11 "parser.mly"
      (Ast.action)
# 138 "parser.ml"
    ))

let _menhir_action_04 =
  fun _1 _3 ->
    (
# 51 "parser.mly"
                              ( LeftCombine (_1 ^ "<-" ^ _3) )
# 146 "parser.ml"
     : (
# 11 "parser.mly"
      (Ast.action)
# 150 "parser.ml"
    ))

let _menhir_action_05 =
  fun _1 _3 ->
    (
# 52 "parser.mly"
                               ( RightCombine (_1 ^ "->" ^ _3) )
# 158 "parser.ml"
     : (
# 11 "parser.mly"
      (Ast.action)
# 162 "parser.ml"
    ))

let _menhir_action_06 =
  fun _1 ->
    (
# 21 "parser.mly"
              ( _1 )
# 170 "parser.ml"
     : (
# 10 "parser.mly"
      (Ast.membrane list)
# 174 "parser.ml"
    ))

let _menhir_action_07 =
  fun () ->
    let _ = (
# 22 "parser.mly"
          ( raise (ParseError) )
# 182 "parser.ml"
     : (
# 10 "parser.mly"
      (Ast.membrane list)
# 186 "parser.ml"
    )) in
    prerr_string "Menhir: misuse: the semantic action associated with the production\nmain -> error\nis expected to abort the parser, but does not do so.\n";
    assert false

let _menhir_action_08 =
  fun _2 ->
    (
# 29 "parser.mly"
                            ( MoleculeMembrane _2 )
# 196 "parser.ml"
     : (
# 12 "parser.mly"
      (Ast.membrane)
# 200 "parser.ml"
    ))

let _menhir_action_09 =
  fun _1 ->
    (
# 25 "parser.mly"
             ( [_1] )
# 208 "parser.ml"
     : (
# 13 "parser.mly"
      (Ast.membrane list)
# 212 "parser.ml"
    ))

let _menhir_action_10 =
  fun _1 _3 ->
    (
# 26 "parser.mly"
                             ( _1 :: _3 )
# 220 "parser.ml"
     : (
# 13 "parser.mly"
      (Ast.membrane list)
# 224 "parser.ml"
    ))

let _menhir_action_11 =
  fun _1 ->
    (
# 36 "parser.mly"
            ( ProcessMolecule _1 )
# 232 "parser.ml"
     : (
# 14 "parser.mly"
      (Ast.molecule)
# 236 "parser.ml"
    ))

let _menhir_action_12 =
  fun _1 ->
    (
# 37 "parser.mly"
             ( ResourceMolecule _1 )
# 244 "parser.ml"
     : (
# 14 "parser.mly"
      (Ast.molecule)
# 248 "parser.ml"
    ))

let _menhir_action_13 =
  fun _1 ->
    (
# 32 "parser.mly"
             ( [_1] )
# 256 "parser.ml"
     : (
# 15 "parser.mly"
      (Ast.molecule list)
# 260 "parser.ml"
    ))

let _menhir_action_14 =
  fun _1 _3 ->
    (
# 33 "parser.mly"
                             ( _1 :: _3 )
# 268 "parser.ml"
     : (
# 15 "parser.mly"
      (Ast.molecule list)
# 272 "parser.ml"
    ))

let _menhir_action_15 =
  fun _1 _3 ->
    (
# 40 "parser.mly"
                       ( ActionProcess (_1, _3) )
# 280 "parser.ml"
     : (
# 16 "parser.mly"
      (Ast.process)
# 284 "parser.ml"
    ))

let _menhir_action_16 =
  fun _1 ->
    (
# 41 "parser.mly"
           ( ActionProcess (_1, NullProcess) )
# 292 "parser.ml"
     : (
# 16 "parser.mly"
      (Ast.process)
# 296 "parser.ml"
    ))

let _menhir_action_17 =
  fun _1 ->
    (
# 44 "parser.mly"
          ( SimpleResource _1 )
# 304 "parser.ml"
     : (
# 17 "parser.mly"
      (Ast.resource)
# 308 "parser.ml"
    ))

let _menhir_action_18 =
  fun () ->
    (
# 45 "parser.mly"
      ( NullResource )
# 316 "parser.ml"
     : (
# 17 "parser.mly"
      (Ast.resource)
# 320 "parser.ml"
    ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | BANG ->
        "BANG"
    | COMMA ->
        "COMMA"
    | DOT ->
        "DOT"
    | EOF ->
        "EOF"
    | IDENT _ ->
        "IDENT"
    | LBRACE ->
        "LBRACE"
    | LEFTARROW ->
        "LEFTARROW"
    | NU ->
        "NU"
    | O ->
        "O"
    | QUESTION ->
        "QUESTION"
    | RBRACE ->
        "RBRACE"
    | RIGHTARROW ->
        "RIGHTARROW"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState02 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | O ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NU ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_18 () in
      _menhir_goto_resource _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_resource : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_12 _1 in
      _menhir_goto_molecule _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_molecule : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_molecule (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState25 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | O ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NU ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_13 _1 in
          _menhir_goto_molecules _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
          let _menhir_s = MenhirState28 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NU ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RIGHTARROW ->
                  _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
              | QUESTION ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
              | LEFTARROW ->
                  _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
              | BANG ->
                  _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COMMA | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_16 _1 in
          _menhir_goto_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
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
  
  and _menhir_goto_process : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState28 ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState25 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_30 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_action -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_action (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_15 _1 _3 in
      _menhir_goto_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_21 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_goto_molecule _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
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
          let _v = _menhir_action_17 _1 in
          _menhir_goto_resource _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_molecules : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState02 ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_26 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_molecule -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_molecule (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_14 _1 _3 in
      _menhir_goto_molecules _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_22 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LBRACE -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LBRACE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_08 _2 in
      let _menhir_stack = MenhirCell1_membrane (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState33 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | _ ->
          let _v = _menhir_action_07 () in
          MenhirBox_main _v
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
