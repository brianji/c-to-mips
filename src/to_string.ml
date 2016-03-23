open Ast

let prim_string = function
  | Int -> "int"
  | Float -> "float"
  | Char -> "char"

let return_string = function
  | Void -> "void"
  | Prim p -> prim_string p

let inop_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Mod -> "%"
  | Asgmt -> "="
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | Less -> "<"
  | LesserEq -> "<="
  | Greater -> ">"
  | GreaterEq -> ">="
  | Equals -> "=="
  | NotEquals -> "!="
  | BitAnd -> "&"
  | BitXor -> "^"
  | BitOr -> "|"
  | And -> "&&"
  | Or -> "||"
  | PlusA -> "+="
  | MinusA -> "-="
  | TimesA -> "*="
  | DivideA -> "/="
  | ModA -> "%="
  | ShiftLeftA -> "<<="
  | ShiftRightA -> ">>="
  | BitAndA -> "&="
  | BitOrA -> "|="
  | BitXorA -> "^="
  | Comma -> ","

let endop_string = function
  | Incrmt -> "++"
  | Decrmt -> "--"
  | Not -> "!"
  | Comp -> "~"

let value_string = function
  | Integer i -> string_of_int i
  | Decimal d -> string_of_float d
  | Letter l -> String.make 1 l

let param_string (prim, id) = prim_string prim ^ " " ^ id

let rec params_string = function
  | [] -> ""
  | [h] -> param_string h
  | h :: t -> param_string h ^ ", " ^ params_string t

let rec expr_string = function
  | Empty -> ""
  | Var v -> v
  | Value v -> value_string v
  | Paren e -> "(" ^ expr_string e ^ ")"
  | FunctionCall (id, args) -> id ^ "(" ^ args_string args ^ ")"
  | Infix (e1, i, e2) ->
    let op_string = match i with
      | Comma -> ", "
      | _ -> " " ^ inop_string i ^ " "
    in
    expr_string e1 ^ op_string ^ expr_string e2
  | Prefix (e, id) -> endop_string e ^ id
  | Postfix (id, e) -> id ^ endop_string e
and args_string = function
  | [] -> ""
  | [h] -> expr_string h
  | h :: t -> expr_string h ^ ", " ^ args_string t

let rec decs_string = function
  | [] -> ""
  | [h] -> expr_string h
  | h :: t -> expr_string h ^ ", " ^ decs_string t

let function_string (return, id, params, block) = 
