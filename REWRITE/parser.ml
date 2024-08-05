
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

# 37 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState01 : (('s, _menhir_box_main) _menhir_cell1_LBRACE, _menhir_box_main) _menhir_state
    (** State 01.
        Stack shape : LBRACE.
        Start symbol: main. *)

  | MenhirState24 : (('s, _menhir_box_main) _menhir_cell1_molecule, _menhir_box_main) _menhir_state
    (** State 24.
        Stack shape : molecule.
        Start symbol: main. *)

  | MenhirState27 : (('s, _menhir_box_main) _menhir_cell1_action, _menhir_box_main) _menhir_state
    (** State 27.
        Stack shape : action.
        Start symbol: main. *)

  | MenhirState32 : (('s, _menhir_box_main) _menhir_cell1_membrane, _menhir_box_main) _menhir_state
    (** State 32.
        Stack shape : membrane.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_action = 
  | MenhirCell1_action of 's * ('s, 'r) _menhir_state * (
# 18 "parser.mly"
      (Ast.action)
# 70 "parser.ml"
)

and ('s, 'r) _menhir_cell1_membrane = 
  | MenhirCell1_membrane of 's * ('s, 'r) _menhir_state * (
# 13 "parser.mly"
      (Ast.membrane)
# 77 "parser.ml"
)

and ('s, 'r) _menhir_cell1_molecule = 
  | MenhirCell1_molecule of 's * ('s, 'r) _menhir_state * (
# 15 "parser.mly"
      (Ast.molecule)
# 84 "parser.ml"
)

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * (
# 5 "parser.mly"
       (string)
# 91 "parser.ml"
)

and ('s, 'r) _menhir_cell1_LBRACE = 
  | MenhirCell1_LBRACE of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (
# 10 "parser.mly"
      (Ast.membrane)
# 101 "parser.ml"
) [@@unboxed]

let _menhir_action_01 =
  fun _2 ->
    (
# 49 "parser.mly"
                 ( NewChannel _2 )
# 109 "parser.ml"
     : (
# 18 "parser.mly"
      (Ast.action)
# 113 "parser.ml"
    ))

let _menhir_action_02 =
  fun _1 _3 ->
    (
# 50 "parser.mly"
                         ( Send (_1 ^ "!" ^ _3) )
# 121 "parser.ml"
     : (
# 18 "parser.mly"
      (Ast.action)
# 125 "parser.ml"
    ))

let _menhir_action_03 =
  fun _1 _3 ->
    (
# 51 "parser.mly"
                             ( Receive (_1 ^ "?" ^ _3) )
# 133 "parser.ml"
     : (
# 18 "parser.mly"
      (Ast.action)
# 137 "parser.ml"
    ))

let _menhir_action_04 =
  fun _1 _3 ->
    (
# 52 "parser.mly"
                              ( LeftCombine (_1 ^ "<-" ^ _3) )
# 145 "parser.ml"
     : (
# 18 "parser.mly"
      (Ast.action)
# 149 "parser.ml"
    ))

let _menhir_action_05 =
  fun _1 _3 ->
    (
# 53 "parser.mly"
                               ( RightCombine (_1 ^ "->" ^ _3) )
# 157 "parser.ml"
     : (
# 18 "parser.mly"
      (Ast.action)
# 161 "parser.ml"
    ))

let _menhir_action_06 =
  fun _1 ->
    (
# 23 "parser.mly"
              ( MoleculeMembrane (List.concat _1) )
# 169 "parser.ml"
     : (
# 10 "parser.mly"
      (Ast.membrane)
# 173 "parser.ml"
    ))

let _menhir_action_07 =
  fun _2 ->
    (
# 30 "parser.mly"
                            ( MoleculeMembrane _2 )
# 181 "parser.ml"
     : (
# 13 "parser.mly"
      (Ast.membrane)
# 185 "parser.ml"
    ))

let _menhir_action_08 =
  fun _1 ->
    (
# 26 "parser.mly"
             ( [_1] )
# 193 "parser.ml"
     : (
# 12 "parser.mly"
      (Ast.membrane list)
# 197 "parser.ml"
    ))

let _menhir_action_09 =
  fun _1 _3 ->
    (
# 27 "parser.mly"
                             ( _1 :: _3 )
# 205 "parser.ml"
     : (
# 12 "parser.mly"
      (Ast.membrane list)
# 209 "parser.ml"
    ))

let _menhir_action_10 =
  fun _1 ->
    (
# 37 "parser.mly"
            ( ProcessMolecule _1 )
# 217 "parser.ml"
     : (
# 15 "parser.mly"
      (Ast.molecule)
# 221 "parser.ml"
    ))

let _menhir_action_11 =
  fun _1 ->
    (
# 38 "parser.mly"
             ( ResourceMolecule _1 )
# 229 "parser.ml"
     : (
# 15 "parser.mly"
      (Ast.molecule)
# 233 "parser.ml"
    ))

let _menhir_action_12 =
  fun _1 ->
    (
# 33 "parser.mly"
             ( [_1] )
# 241 "parser.ml"
     : (
# 14 "parser.mly"
      (Ast.molecule list)
# 245 "parser.ml"
    ))

let _menhir_action_13 =
  fun _1 _3 ->
    (
# 34 "parser.mly"
                             ( _1 :: _3 )
# 253 "parser.ml"
     : (
# 14 "parser.mly"
      (Ast.molecule list)
# 257 "parser.ml"
    ))

let _menhir_action_14 =
  fun _1 _3 ->
    (
# 41 "parser.mly"
                       ( ActionProcess (_1, _3) )
# 265 "parser.ml"
     : (
# 16 "parser.mly"
      (Ast.process)
# 269 "parser.ml"
    ))

let _menhir_action_15 =
  fun _1 ->
    (
# 42 "parser.mly"
           ( ActionProcess (_1, NullProcess) )
# 277 "parser.ml"
     : (
# 16 "parser.mly"
      (Ast.process)
# 281 "parser.ml"
    ))

let _menhir_action_16 =
  fun _1 ->
    (
# 45 "parser.mly"
          ( SimpleResource _1 )
# 289 "parser.ml"
     : (
# 17 "parser.mly"
      (Ast.resource)
# 293 "parser.ml"
    ))

let _menhir_action_17 =
  fun () ->
    (
# 46 "parser.mly"
      ( NullResource )
# 301 "parser.ml"
     : (
# 17 "parser.mly"
      (Ast.resource)
# 305 "parser.ml"
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
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState01 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | O ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NU ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_17 () in
      _menhir_goto_resource _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_resource : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_goto_molecule _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_molecule : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_molecule (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState24 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | O ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NU ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_12 _1 in
          _menhir_goto_molecules _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
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
          let _menhir_s = MenhirState27 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NU ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RIGHTARROW ->
                  _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer
              | QUESTION ->
                  _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
              | LEFTARROW ->
                  _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
              | BANG ->
                  _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COMMA | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_15 _1 in
          _menhir_goto_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_main =
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
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_main =
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
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_main =
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
  
  and _menhir_run_16 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _menhir_box_main =
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
      | MenhirState27 ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState24 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_29 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_action -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_action (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_14 _1 _3 in
      _menhir_goto_process _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_20 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_10 _1 in
      _menhir_goto_molecule _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHTARROW ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer
      | QUESTION ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LEFTARROW ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BANG ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_16 _1 in
          _menhir_goto_resource _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_molecules : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState24 ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState01 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_25 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_molecule -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_molecule (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_13 _1 _3 in
      _menhir_goto_molecules _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_21 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LBRACE -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LBRACE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_07 _2 in
      let _menhir_stack = MenhirCell1_membrane (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState32 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
