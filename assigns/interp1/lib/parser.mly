%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token TRUE FALSE
%token IF THEN ELSE
%token LET IN FUN
%token LPAREN RPAREN
%token EQ NEQ LT LTE GT GTE
%token PLUS MINUS TIMES DIV MOD
%token AND OR
%token ARROW
%token EOF

%start <Utils.prog> prog
%%

prog:
  | e = expr; EOF { e }

expr:
  | IF expr THEN expr ELSE expr     { If ($2, $4, $6) }
  | LET VAR EQ expr IN expr         { Let ($2, $4, $6) }
  | FUN VAR ARROW expr              { Fun ($2, $4) }
  | expr2                           { $1 }

expr2:
  | expr2 bop expr2                 { Bop ($2, $1, $3) }
  | expr3 expr3                     { App ($1, $2) }
  | expr3                           { $1 }

expr3:
  | NUM                             { Num $1 }
  | TRUE                            { True }
  | FALSE                           { False }
  | VAR                             { Var $1 }
  | LPAREN expr RPAREN              { $2 }

bop:
  | PLUS   { Add }
  | MINUS  { Sub }
  | TIMES  { Mul }
  | DIV    { Div }
  | MOD    { Mod }
  | LT     { Lt }
  | LTE    { Lte }
  | GT     { Gt }
  | GTE    { Gte }
  | EQ     { Eq }
  | NEQ    { Neq }
  | AND    { And }
  | OR     { Or }
