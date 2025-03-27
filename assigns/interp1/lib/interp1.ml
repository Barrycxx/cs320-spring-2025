include Utils

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

(* partial stubs — required only to pass build, not correctness *)
let subst (_ : value) (_ : string) (_ : expr) : expr = Unit

let eval (_ : expr) : (value, error) result = Ok VUnit

let interp (_ : string) : (value, error) result = Ok VUnit
