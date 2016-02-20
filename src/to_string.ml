open Ast

let prim_string = function
  | Int -> "int"
  | Float -> "float"
  | Char -> "char"

let return_string = function
  | Void -> "void"
  | Prim p -> prim_string p

let param_string = function
  (prim, id) -> (prim_string prim) ^ " " ^ id
