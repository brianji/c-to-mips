open Ast

let indent_string i = String.make (2 * i) ' '

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
  | Comma -> ","

let asop_string = function
  | Asgmt -> "="
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

let endop_string = function
  | Incrmt -> "++"
  | Decrmt -> "--"
  | Not -> "!"
  | Comp -> "~"
  | Pos -> "+"
  | Neg -> "-"

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
  | Assign (e1, i, e2) -> e1 ^ " " ^ asop_string i ^ " " ^ expr_string e2
  | Prefix (op, e) -> endop_string op ^ expr_string e
  | Postfix (e, op) -> expr_string e ^ endop_string op
and args_string = function
  | [] -> ""
  | [h] -> expr_string h
  | h :: t -> expr_string h ^ ", " ^ args_string t

let rec decs_string = function
  | [] -> ""
  | [h] -> expr_string h
  | h :: t -> expr_string h ^ ", " ^ decs_string t

let rec statements_string statements indent = match statements with
  | [] -> ""
  | h :: t ->
    indent_string indent
      ^ statement_string h indent
      ^ statements_string t indent
and statement_string statement indent = match statement with
  | Dec (p, decs) -> prim_string p ^ " " ^ decs_string decs ^ ";\n"
  | Expr e -> expr_string e ^ ";\n"
  | Return -> "return;\n"
  | ReturnExpr e -> "return " ^ expr_string e ^ ";\n"
  | Break -> "break;\n"
  | Continue -> "continue;\n"
  | Block b ->
    "{\n" ^ statements_string b (indent + 1) ^ indent_string indent ^ "}"
  | While (e, s) ->
    "while (" ^ expr_string e ^ ") " ^ statement_string s indent ^ "\n"
  | For ((e1, e2, e3), s) ->
    "for ("
      ^ expr_string e1
      ^ "; "
      ^ expr_string e2
      ^ "; "
      ^ expr_string e3
      ^ statement_string s indent
      ^ "\n"
  | If (e, s) ->
    "if (" ^ expr_string e ^ ") " ^ statement_string s indent ^ "\n"
  | IfElse (e, s1, s2) ->
    let end_string = match s2 with
      | If _ -> ""
      | IfElse _ -> ""
      | _ -> "\n"
    in
    "if ("
      ^ expr_string e
      ^ ") "
      ^ statement_string s1 indent
      ^ " else "
      ^ statement_string s2 indent
      ^ end_string

let function_string (return, id, params, block) =
  return_string return
    ^ " "
    ^ id
    ^ "("
    ^ params_string params
    ^ ") "
    ^ statement_string block 0
    ^ "\n"

let rec prog_string = function
  | [] -> ""
  | h :: t -> function_string h ^ prog_string t

let _ =
  let s = Lexing.from_channel stdin |> Parser.prog Lexer.read |> prog_string
  in print_string s
