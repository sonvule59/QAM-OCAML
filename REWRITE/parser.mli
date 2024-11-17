
(* The type of tokens. *)

type token = 
  | RIGHTARROW
  | RBRACE
  | QUESTION
  | O
  | NU
  | LEFTARROW
  | LBRACE
  | IDENT of (string)
  | EOF
  | DOT
  | COMMA
  | BANG

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.membrane list)
