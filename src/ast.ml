type prog = func list
and func = return * id * (param list) * statement
and id = string
and return =
  | Void
  | Prim of prim
and prim =
  | Int
  | Float
  | Char
and value =
  | Integer of int
  | Decimal of float
  | Letter of char
and param = prim * id
and statement =
  | Expr of expr
  | Dec of prim * (expr list)
  | Return
  | ReturnExpr of expr
  | Break
  | Continue
  | Block of statement list
  | While of expr * statement
  | For of (expr * expr * expr) * statement
  | If of expr * statement
  | IfElse of expr * statement * statement
and dec_expr =
  | DecVar of id
  | InitVar of id * expr
and expr =
  | Empty
  | Var of id
  | Value of value
  | Paren of expr
  | FunctionCall of id * (expr list)
  | Infix of expr * inop * expr
  | Assign of id * asop * expr
  | Prefix of endop * expr
  | Postfix of expr * endop
and inop =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
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
  | Comma
and asop =
  | Asgmt
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
and endop =
  | Incrmt
  | Decrmt
  | Not
  | Comp
