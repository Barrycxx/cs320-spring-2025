include Utils

(* Implements parse using the provided parser and lexer.
   This is the only part required for the one-week check-in. *)
let parse (s : string) : prog option =
  try Some (Parser.prog Lexer.read (Lexing.from_string s))
  with _ -> None

(* The rest of the functions are not needed for the check-in.
   They are left as stubs so the code compiles. *)

let subst (_ : value) (_ : string) (_ : expr) : expr = Unit

let eval (_ : expr) : (value, error) result = Ok VUnit

let interp (_ : string) : (value, error) result = Ok VUnit
