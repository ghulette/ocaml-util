%{
%}

%token <string> ID
%token BEGIN
%token END
%token LAMBDA
%token ARROW
%token EOF
%start main
%type <(Little.t option)> main

%%

main:
  | expr EOF { Some $1 }
  |          { None }
;

expr:
  | BEGIN expr END { $2 }
  | ID { Little.Var $1 }
  | LAMBDA ID ARROW expr { Little.Func ($2,$4) }
  | expr expr { Little.App ($1,$2) }
;
