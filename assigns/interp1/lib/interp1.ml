include Utils

let parse (s : string) : expr option =
  try
    let lexbuf = Lexing.from_string s in
    Some (Parser.prog Lexer.read lexbuf)
  with _ -> None

let subst (_ : value) (_ : string) (_ : expr) : expr =
  Unit

let eval (_ : expr) : (value, error) result =
  Ok VUnit

let interp (_ : string) : (value, error) result =
  Ok VUnit
