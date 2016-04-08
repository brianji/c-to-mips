type prog = prog_elmt list
and prog_elmt =
  | Func of func
  | Global of dec
and func = prim * id * (param list) * statement
and dec = prim * (expr list)
and id = string
and prim =
  | Void
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
  | Dec of dec
  | Return
  | ReturnExpr of expr
  | Break
  | Continue
  | Block of block
  | While of while_loop
  | For of for_loop
  | If of if_statement
  | IfElse of if_else_statement
and block = statement list
and while_loop = expr * statement
and for_loop = (expr * expr * expr) * statement
and if_statement = expr * statement
and if_else_statement = expr * statement * statement
and dec_expr =
  | DecVar of id
  | InitVar of id * expr
and expr =
  | Empty
  | Var of id
  | Value of value
  | Paren of expr
  | FunctionCall of function_call
  | Infix of infix
  | Assign of assign
  | Prefix of prefix
  | Postfix of postfix
and function_call = id * (expr list)
and infix = expr * inop * expr
and assign = id * asop * expr
and prefix = endop * expr
and postfix = expr * endop
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
  | Pos
  | Neg
