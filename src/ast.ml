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
  | Dec of prim * expr
  | Return
  | ReturnExpr of expr
  | While of expr * (statement list)
and dec_expr =
  | DecVar of id
  | InitVar of id * expr
and expr =
  | Var of id
  | Value of value
  | Infix of expr * inop * expr
  | Prefix of endop * id
  | Postfix of id * endop
and inop =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Asgmt
  | ShiftLeft
  | ShiftRight
  | Less
  | LesserEq
  | Greater
  | GreaterEq
  | Equals
  | NotEquals
  | BitAnd
  | BitXor
  | BitOr
  | And
  | Or
  | PlusA
  | MinusA
  | TimesA
  | DivideA
  | ModA
  | ShiftLeftA
  | ShiftRightA
  | BitAndA
  | BitOrA
  | BitXorA
  | Comma
and endop =
  | Incrmt
  | Decrmt
and value =
  | Integer of int
  | Decimal of float
  | Letter of char
