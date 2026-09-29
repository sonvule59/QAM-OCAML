{
  open Parser
  exception SyntaxError of string
}

rule token = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | "+"
    { PLUS }
  | "|[" { AIRLOCK_L }
  | "]|" { AIRLOCK_R }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | "." { DOT }
  | "!" { BANG }
  | "?" { QUESTION }
  | "<-" { LEFTARROW }
  | "->" { RIGHTARROW }
  | "&" { AMP }
  | "0" { ZERO }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
      match id with
      | "nu" -> NU
      | "o" -> O
      | "repl" -> REP
      | _ -> IDENT id
    }
  | eof { EOF }
  | _
    {
      raise
        (SyntaxError
           ("Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'"))
    }
