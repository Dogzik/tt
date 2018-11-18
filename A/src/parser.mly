%{
open Grammar;;
%}
%token <string> VAR
%token LAMBDA DOT 
%token OPEN CLOSE
%token EOF
%nonassoc LAMBDA DOT
%start main
%type <Grammar.expression> main
%%
main:
        exp EOF                 { $1 }
exp:
        apl LAMBDA VAR DOT exp  { Apl($1, Lambda(Name($3), $5)) }
        |LAMBDA VAR DOT exp     { Lambda(Name($2), $4) }
        | apl                   { $1 }
apl:
        apl atom                { Apl($1, $2) }
        |atom                   { $1 }
atom:
        OPEN exp CLOSE          { $2 }
        |VAR                    { Var (Name($1)) }

