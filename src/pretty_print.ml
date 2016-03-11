open Ast
open To_string

let indent = 2

let tail l = try List.tl l with Failure _ -> []

let print_param (prim, id) =
  print_string (prim_string prim);
  print_string " ";
  print_string id

let print_params params =
  let aux a p =
    print_param p;
    match a with
      | [] -> []
      | _ :: t ->
        print_string ", ";
        t
  in
  let _ = List.fold_left aux (tail params) params in ()

let print_inop i = match i with
  | Comma ->
    print_string (inop_string i);
    print_string " "
  | _ ->
    print_string " ";
    print_string (inop_string i);
    print_string " "

let rec print_expr = function
  | Var v -> print_string v
  | Value v -> print_string (value_string v)
  | Infix (e1, i, e2) ->
    print_expr e1;
    print_inop i;
    print_expr e2
  | Prefix (e, id) ->
    print_string (endop_string e);
    print_string id
  | Postfix (id, e) ->
    print_string id;
    print_string (endop_string e)

let print_statement = function
  | Dec (p, e) ->
    print_string (prim_string p);
    print_string " ";
    print_expr e;
    print_string ";";
    print_newline ()
  | Expr e ->
    print_expr e;
    print_string ";";
    print_newline ()

let print_indent i =
  let s = String.make (indent * i) ' ' in
  print_string s

let print_statements statements indent =
  let aux _ statement =
    print_indent indent;
    print_statement statement
  in
  List.fold_left aux () statements

let print_function f =
  let (return, id, params, statements) = f in
  print_string (return_string return);
  print_string " ";
  print_string id;
  print_string "(";
  print_params params;
  print_string ") {";
  print_newline ();
  print_statements statements 1;
  print_string "}";
  print_newline ()

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let f = Parser.main Lexer.read lexbuf in
  print_function f;
  exit 0
