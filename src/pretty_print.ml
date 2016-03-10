open Ast
open To_string

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

let print_inop i =
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

let print_dec_expr = function
  | DecVar id -> print_string id
  | InitVar (id, expr) ->
    print_string id;
    print_string " = ";
    print_expr expr

let print_dec (p, l) =
  print_string (prim_string p);
  print_string " ";
  let aux a dec =
    print_dec_expr dec;
    match a with
      | [] -> []
      | _ :: t ->
        print_string ", ";
        t
  in
  let _ = List.fold_left aux (tail l) l in
  print_string ";";
  print_newline ()

let print_statement = function
  | Dec (p, l) -> print_dec (p, l)
  | Expr e ->
    print_expr e;
    print_string ";";
    print_newline ()

let print_statements statements =
  let aux _ statement = print_statement statement in
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
  print_statements statements;
  print_string "}";
  print_newline ()

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let f = Parser.main Lexer.read lexbuf in
  print_function f;
  exit 0
