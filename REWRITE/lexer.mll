(* {
exception SyntaxError of string
open Parser
} *)
{
  open Parser  (* This should match the name of your parser module *)
  exception SyntaxError of string
}


rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf } (* Skip whitespace *)
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | "." { DOT }
  | "!" { BANG }
  | "?" { QUESTION }
  | "<-" { LEFTARROW }
  | "->" { RIGHTARROW }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
      match id with
      | "nu" -> NU
      | "o" -> O
      | _ -> IDENT id
    }
  | eof { EOF }
  | _ { raise (SyntaxError "Unknown character") }
