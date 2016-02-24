type func = return * id * (param list) * (statement list)
and id = string
and return =
  | Void
  | Prim of prim
and prim =
  | Int
  | Float
  | Char
and param = prim * id
and statement =
  | Expr of expr
  | Dec of prim * (dec_expr list)
and dec_expr =
  | DecVar of id
  | InitVar of id * expr
and expr =
  | Var of var
  | Value of value
  | Infix of expr * inop * expr
  | Prefix of endop * expr
  | Postfix of expr * endop
and inop =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Asgmt
and endop =
  | Incrmt
  | Decrmt
and var = string * value
and value =
  | Integer of int
  | Decimal of float
  | Letter of char
