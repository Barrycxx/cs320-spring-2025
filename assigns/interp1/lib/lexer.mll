{
open Parser
}

let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let digits = digit+
let number = '-'? digits
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let symbol = ['_' '\'']
let id = (lowercase | '_') (lowercase | uppercase | digit | symbol)*

rule read = parse
  | whitespace { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "()" { UNIT }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "mod" { MOD }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "=" { EQ }
  | "<>" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | number as n { NUM (int_of_string n) }
  | id as x { VAR x }
  | eof { EOF }
