include Utils

let parse (s : string) : expr option =
  try
    let lexbuf = Lexing.from_string s in
    Some (Parser.prog Lexer.read lexbuf)
  with _ -> None

let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Unit -> Unit
  | True -> True
  | False -> False
  | Num _ -> e
  | Var y -> if x = y then
               match v with
               | VNum n -> Num n
               | VBool b -> if b then True else False
               | VUnit -> Unit
               | VFun (arg, body) -> Fun(arg, body)
             else Var y
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) ->
      let e1' = subst v x e1 in
      if x = y then Let (y, e1', e2)
      else Let (y, e1', subst v x e2)
  | Fun (y, e1) ->
      if x = y then Fun (y, e1)
      else Fun (y, subst v x e1)

let rec eval (e : expr) : (value, error) result =
  match e with
  | Unit -> Ok VUnit
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Num n -> Ok (VNum n)
  | Var x -> Error (UnknownVar x)
  | Fun (x, e) -> Ok (VFun (x, e))
  | App (e1, e2) ->
      (match eval e1 with
       | Ok (VFun (x, body)) ->
           (match eval e2 with
            | Ok v2 -> eval (subst v2 x body)
            | Error err -> Error err)
       | Ok _ -> Error InvalidApp
       | Error err -> Error err)
  | Bop (op, e1, e2) ->
      (match eval e1 with
       | Error err -> Error err
       | Ok v1 ->
         (match eval e2 with
          | Error err -> Error err
          | Ok v2 ->
            match (op, v1, v2) with
            | (Add, VNum n1, VNum n2) -> Ok (VNum (n1 + n2))
            | (Sub, VNum n1, VNum n2) -> Ok (VNum (n1 - n2))
            | (Mul, VNum n1, VNum n2) -> Ok (VNum (n1 * n2))
            | (Div, VNum _, VNum 0) -> Error DivByZero
            | (Div, VNum n1, VNum n2) -> Ok (VNum (n1 / n2))
            | (Mod, VNum _, VNum 0) -> Error DivByZero
            | (Mod, VNum n1, VNum n2) -> Ok (VNum (n1 mod n2))
            | (Lt, VNum n1, VNum n2) -> Ok (VBool (n1 < n2))
            | (Lte, VNum n1, VNum n2) -> Ok (VBool (n1 <= n2))
            | (Gt, VNum n1, VNum n2) -> Ok (VBool (n1 > n2))
            | (Gte, VNum n1, VNum n2) -> Ok (VBool (n1 >= n2))
            | (Eq, VNum n1, VNum n2) -> Ok (VBool (n1 = n2))
            | (Neq, VNum n1, VNum n2) -> Ok (VBool (n1 <> n2))
            | (And, VBool b1, VBool b2) -> Ok (VBool (b1 && b2))
            | (Or, VBool b1, VBool b2) -> Ok (VBool (b1 || b2))
            | _ -> Error (InvalidArgs op)))
  | If (e1, e2, e3) ->
      (match eval e1 with
       | Ok (VBool true) -> eval e2
       | Ok (VBool false) -> eval e3
       | Ok _ -> Error InvalidIfCond
       | Error err -> Error err)
  | Let (x, e1, e2) ->
      let e1_eval =
        match e1 with
        | Fun _ -> subst (VFun (x, e1)) x e1 (* extra credit *)
        | _ -> e1
      in
      match eval e1_eval with
      | Ok v1 -> eval (subst v1 x e2)
      | Error err -> Error err

let interp (s : string) : (value, error) result =
  match parse s with
  | Some e -> eval e
  | None -> Error ParseFail
