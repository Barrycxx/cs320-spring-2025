{
open Parser
exception Error of string
}

let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let digits = digit+
let num = '-'? digits
let lowercase = ['a'-'z']
let letter = ['a'-'z' 'A'-'Z']
let ident_char = letter | digit | '_' | '\''
let ident = (lowercase | '_') ident_char*

rule read = parse
  | whitespace         { read lexbuf }
  | "true"             { TRUE }
  | "false"            { FALSE }
  | "if"               { IF }
  | "then"             { THEN }
  | "else"             { ELSE }
  | "let"              { LET }
  | "in"               { IN }
  | "fun"              { FUN }
  | "->"               { ARROW }
  | "="                { EQ }
  | "<>"               { NEQ }
  | "<="               { LTE }
  | "<"                { LT }
  | ">="               { GTE }
  | ">"                { GT }
  | "+"                { PLUS }
  | "-"                { MINUS }
  | "*"                { TIMES }
  | "/"                { DIV }
  | "mod"              { MOD }
  | "&&"               { AND }
  | "||"               { OR }
  | "("                { LPAREN }
  | ")"                { RPAREN }
  | num as n           { NUM (int_of_string n) }
  | ident as id        { VAR id }
  | eof                { EOF }
  | _                  { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
