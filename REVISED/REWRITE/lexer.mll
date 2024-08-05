{
open Parser

let keyword_or_identifier = parser
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id ->
    match id with
    | "nu" -> NU
    | "o" -> O
    | _ -> IDENT id
}


rule read = parse
  | [' ' '\t' '\n' '\r'] { read lexbuf } (* Skip whitespace *)
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "," { COMMA }
  | "." { DOT }
  | "!" { BANG }
  | "?" { QUESTION }
  | "<-" { LEFTARROW }
  | "->" { RIGHTARROW }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { keyword_or_identifier (Lexing.from_string id) }
  | eof { EOF }
  | _ { failwith "Unknown character" }
