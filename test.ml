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

let print_params params =
  let param_strings = List.map param_string params in
  let rec aux = function
    | [] -> ()
    | [h] -> print_string h
    | h :: t ->
      let () = print_string (h ^ ", ") in
      aux t
  in
  aux param_strings

let print_statement = ()

let print_statements = ()

let print_function f =
  let (return, id, params, statements) = f in
  let () = print_string (return_string return) in
  let () = print_string " " in
  let () = print_string id in
  let () = print_string "(" in
  let () = print_params params in
  let () = print_string ") {" in
  let () = print_newline () in
  let () = print_statements in
  let () = print_string "}" in
  print_newline()

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let f = Parser.main Lexer.read lexbuf in
  let () = print_function f in
  exit 0
