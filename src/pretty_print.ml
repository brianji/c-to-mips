open Ast
open To_string

let tail l = try List.tl l with Failure _ -> []

let print_indent i = String.make (2 * i) ' ' |> print_string

let print_params params =
  let process_param a (prim, id) =
    print_string @@ prim_string prim;
    print_char ' ';
    print_string id;
    if List.length a > 0 then print_string ", ";
    tail a
  in
  let _ = List.fold_left process_param (tail params) params in ()

let print_inop i = match i with
  | Comma ->
    print_string @@ inop_string i;
    print_char ' '
  | _ ->
    print_char ' ';
    print_string @@ inop_string i;
    print_char ' '

let rec print_expr = function
  | Empty -> ()
  | Var v -> print_string v
  | Value v -> print_string @@ value_string v
  | Infix (e1, i, e2) ->
    print_expr e1;
    print_inop i;
    print_expr e2
  | Prefix (e, id) ->
    print_string @@ endop_string e;
    print_string id
  | Postfix (id, e) ->
    print_string id;
    print_string @@ endop_string e

let rec print_statements statements indent =
  let aux _ statement =
    print_indent indent;
    print_statement statement indent
  in
  List.fold_left aux () statements
and print_statement statement indent = match statement with
  | Dec (p, e) ->
    print_string @@ prim_string p;
    print_char ' ';
    print_expr e;
    print_char ';';
    print_newline ()
  | Expr e ->
    print_expr e;
    print_char ';';
    print_newline ()
  | Return ->
    print_string "return;";
    print_newline ()
  | ReturnExpr e ->
    print_string "return ";
    print_expr e;
    print_char ';';
    print_newline ()
  | While (e, s) ->
    print_string "while (";
    print_expr e;
    print_string ") {";
    print_newline ();
    print_statements s @@ indent + 1;
    print_indent indent;
    print_char '}';
    print_newline ()
  | For ((e1, e2, e3), s) ->
    print_string "for (";
    print_expr e1;
    print_string "; ";
    print_expr e2;
    print_string "; ";
    print_expr e3;
    print_string ") {";
    print_newline ();
    print_statements s @@ indent + 1;
    print_indent indent;
    print_char '}';
    print_newline ()

let print_function (return, id, params, statements) =
  print_string @@ return_string return;
  print_char ' ';
  print_string id;
  print_char '(';
  print_params params;
  print_string ") {";
  print_newline ();
  print_statements statements 1;
  print_char '}';
  print_newline ()

let _ = Lexing.from_channel stdin |> Parser.main Lexer.read |> print_function
