%{
  open Ast
%}

%token <string> IDENT
%token NU O REP ZERO LET
%token LBRACE RBRACE COMMA DOT BANG QUESTION LEFTARROW RIGHTARROW EOF
%token PLUS AIRLOCK_L AIRLOCK_R AMP EQUALS SEMI

%right REP
%left PLUS
%left AMP

%start main

%type <(string * Ast.process) list * Ast.membrane list> main

%%

main:
  | definitions membranes EOF { ($1, $2) }
  | error { raise Parse_error }

definitions:
  | { [] }
  | definitions LET IDENT EQUALS process SEMI { $1 @ [ ($3, $5) ] }

membranes:
  | membrane { [$1] }
  | membrane COMMA membranes { $1 :: $3 }

membrane:
  | LBRACE molecules RBRACE { MoleculeMembrane $2 }
  | AIRLOCK_L membrane COMMA resource COMMA membrane AIRLOCK_R { AirlockedMembrane ($2, $4, $6) }

molecules:
  | molecule { [$1] }
  | molecule COMMA molecules { $1 :: $3 }

molecule:
  | process { ProcessMolecule $1 }
  | resource { ResourceMolecule $1 }

process:
  | process PLUS prefix_process { Choice ($1, $3) }
  | prefix_process { $1 }

prefix_process:
  | REP prefix_process { Replication $2 }
  | atomic_process { $1 }

atomic_process:
  | ZERO { NullProcess }
  | action DOT prefix_process { ActionProcess ($1, $3) }
  | action DOT IDENT { ActionProcess ($1, ProcessRef $3) }
  | action DOT { ActionProcess ($1, NullProcess) }

resource:
  | resource AMP resource { MeetOperation ($1, $3) }
  | atomic_resource { $1 }

atomic_resource:
  | IDENT { SimpleResource $1 }
  | O { NullResource }

action:
  | NU IDENT { mk_action ~loc:$startpos (NewChannel $2) }
  | IDENT BANG IDENT { mk_action ~loc:$startpos (Send { chan = $1; arg = Named $3 }) }
  | IDENT QUESTION IDENT { mk_action ~loc:$startpos (Receive { chan = $1; arg = $3 }) }
  | IDENT LEFTARROW IDENT { mk_action ~loc:$startpos (Encode { chan = $1; msg = Named $3 }) }
  | IDENT RIGHTARROW IDENT { mk_action ~loc:$startpos (Decode { chan = $1; binder = $3 }) }
