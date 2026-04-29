
(* The type of tokens. *)

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
  | IDENT of (string)
  | EOF
  | DOT
  | COMMA
  | BANG
  | AIRLOCK_R
  | AIRLOCK_L

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.membrane list)
