open Ast
open To_string

let tail l = try List.tl l with Failure _ -> []

let print_indent i = String.make (2 * i) ' ' |> print_string

let print_params params = print_string @@ params_string params

let print_expr e = print_string @@ expr_string e

let print_decs decs = print_string @@ decs_string decs

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
