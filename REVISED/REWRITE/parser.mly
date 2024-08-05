%{
open Ast
%}

%token <string> IDENT
%token NU O
%token LBRACE RBRACE COMMA DOT BANG QUESTION LEFTARROW RIGHTARROW EOF

%start main
%type <membrane> main

%%

main:
  | membranes { MoleculeMembrane (List.concat $1) }

membranes:
  | membrane { [$1] }
  | membrane COMMA membranes { $1 :: $3 }

membrane:
  | LBRACE molecules RBRACE { $2 }

molecules:
  | molecule { [$1] }
  | molecule COMMA molecules { $1 :: $3 }

molecule:
  | process { ProcessMolecule $1 }
  | resource { ResourceMolecule $1 }

process:
  | action DOT process { ActionProcess ($1, $3) }
  | action { ActionProcess ($1, NullProcess) }

resource:
  | IDENT { SimpleResource $1 }
  | O { NullResource }

action:
  | NU IDENT DOT { NewChannel $2 }
  | IDENT BANG IDENT DOT { Send ($1 ^ "!" ^ $3) }
  | IDENT QUESTION IDENT DOT { Receive ($1 ^ "?" ^ $3) }
  | IDENT LEFTARROW IDENT DOT { LeftCombine ($1 ^ "<-" ^ $3) }
  | IDENT RIGHTARROW IDENT DOT { RightCombine ($1 ^ "->" ^ $3) }
