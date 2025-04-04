%{
open Utils
%}

%token TRUE FALSE
%token UNIT
%token IF THEN ELSE
%token LET IN FUN
%token ARROW
%token LPAREN RPAREN
%token <int> NUM
%token <string> VAR
%token EOF 

/* Binary operators */
%token PLUS MINUS TIMES DIV MOD
%token LT LTE GT GTE EQ NEQ
%token AND OR

%start <Utils.prog> prog

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%left APP

%%

prog:
  | e = expr EOF { e }

expr:
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { If(e1, e2, e3) }
  | LET x = VAR EQ e1 = expr IN e2 = expr { Let(x, e1, e2) }
  | FUN x = VAR ARROW e = expr { Fun(x, e) }
  | e = expr2 { e }

expr2:
  | e1 = expr2 PLUS e2 = expr2 { Bop(Add, e1, e2) }
  | e1 = expr2 MINUS e2 = expr2 { Bop(Sub, e1, e2) }
  | e1 = expr2 TIMES e2 = expr2 { Bop(Mul, e1, e2) }
  | e1 = expr2 DIV e2 = expr2 { Bop(Div, e1, e2) }
  | e1 = expr2 MOD e2 = expr2 { Bop(Mod, e1, e2) }
  | e1 = expr2 LT e2 = expr2 { Bop(Lt, e1, e2) }
  | e1 = expr2 LTE e2 = expr2 { Bop(Lte, e1, e2) }
  | e1 = expr2 GT e2 = expr2 { Bop(Gt, e1, e2) }
  | e1 = expr2 GTE e2 = expr2 { Bop(Gte, e1, e2) }
  | e1 = expr2 EQ e2 = expr2 { Bop(Eq, e1, e2) }
  | e1 = expr2 NEQ e2 = expr2 { Bop(Neq, e1, e2) }
  | e1 = expr2 AND e2 = expr2 { Bop(And, e1, e2) }
  | e1 = expr2 OR e2 = expr2 { Bop(Or, e1, e2) }
  | e = expr3 { e }

expr3:
  | e1 = expr3 e2 = expr4 %prec APP { App(e1, e2) }
  | e = expr4 { e }

expr4:
  | n = NUM { Num(n) }
  | TRUE { True }
  | FALSE { False }
  | UNIT { Unit }
  | x = VAR { Var(x) }
  | LPAREN e = expr RPAREN { e }
