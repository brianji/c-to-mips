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
  | Paren e ->
    print_char '(';
    print_expr e;
    print_char ')'
  | FunctionCall (id, args) ->
    print_string id;
    print_char '(';
    print_args args;
    print_char ')'
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
and print_args args =
  let process_arg a expr =
    print_expr expr;
    if List.length a > 0 then print_string ", ";
    tail a
  in
  let _ = List.fold_left process_arg (tail args) args in ()

let print_decs decs =
  let process_dec a expr =
    print_expr expr;
    if List.length a > 0 then print_string ", ";
    tail a
  in
  let _ = List.fold_left process_dec (tail decs) decs in ()

let rec print_statements statements indent =
  let aux _ statement =
    print_indent indent;
    print_statement statement indent
  in
  List.fold_left aux () statements
and print_statement statement indent = match statement with
  | Dec (p, decs) ->
    print_string @@ prim_string p;
    print_char ' ';
    print_decs decs;
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
  | Break ->
    print_string "break;";
    print_newline ()
  | Continue ->
    print_string "continue;";
    print_newline ()
  | Block b ->
    print_char '{';
    print_newline ();
    print_statements b @@ indent + 1;
    print_indent indent;
    print_char '}'
  | While (e, s) ->
    print_string "while (";
    print_expr e;
    print_string ") ";
    print_statement s indent;
    print_newline ()
  | For ((e1, e2, e3), s) ->
    print_string "for (";
    print_expr e1;
    print_string "; ";
    print_expr e2;
    print_string "; ";
    print_expr e3;
    print_string ") ";
    print_statement s indent;
    print_newline ()
  | If (e, s) ->
    print_string "if (";
    print_expr e;
    print_string ") ";
    print_statement s indent;
    print_newline ()
  | IfElse (e, s1, s2) ->
    print_string "if (";
    print_expr e;
    print_string ") ";
    print_statement s1 indent;
    print_string " else ";
    print_statement s2 indent;
    match s2 with
      | If _ -> ()
      | IfElse _ -> ()
      | _ -> print_newline ()

let print_function (return, id, params, block) =
  print_string @@ return_string return;
  print_char ' ';
  print_string id;
  print_char '(';
  print_params params;
  print_string ") ";
  print_statement block 0;
  print_newline ()

let _ = Lexing.from_channel stdin |> Parser.main Lexer.read |> print_function
