type bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq | And | Or

type expr =
  | Unit
  | True | False
  | Num of int
  | Var of string
  | App of expr * expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Fun of string * expr

type prog = expr

type value =
  | VNum of int
  | VBool of bool
  | VUnit
  | VFun of string * expr

type error =
  | DivByZero
  | InvalidIfCond
  | InvalidArgs of bop
  | InvalidApp
  | UnknownVar of string
  | ParseFail

let string_of_value (v : value) : string =
  match v with
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VFun (_,_) -> "<fun>"

let string_of_bop = function
  | Add -> "(+)"
  | Sub -> "(-)"
  | Mul -> "(*)"
  | Div -> "(/)"
  | Mod -> "(mod)"
  | Lt -> "(<)"
  | Lte -> "(<=)"
  | Gt -> "(>)"
  | Gte -> "(>=)"
  | Eq -> "(=)"
  | Neq -> "(<>)"
  | And -> "(&&)"
  | Or -> "(||)"

let string_of_error = function
  | DivByZero -> "division by zero"
  | InvalidIfCond -> "non-Boolean value given as condition"
  | InvalidArgs op -> "invalid arguments given to " ^ string_of_bop op
  | InvalidApp -> "non-function value used in function application"
  | UnknownVar x -> "unknown variable \'" ^ x ^ "\'"
  | ParseFail -> "syntax error"
