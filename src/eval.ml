open Ast

let hash_size = 100

let rec eval_expr expr table = match expr with
  | Empty -> table
  | Var v ->
    let _ =
      try Hashtbl.find table v with Not_found -> failwith v ^ " not declared"
    in
    table
  | Value v -> table
  | Paren e -> eval_expr expr table
  | FunctionCall (id, args) -> table
  | Infix (e1, i, e2) -> table
  | Prefix (e, id) -> table
  | Postfix (id, e) -> table

let rec eval_statements statements table = match statements with
  | [] -> ()
  | h :: t -> eval_statement h table
and eval_statement statement table = match statement with
  | Dec (p, decs) -> ()
  | Expr e -> ()
  | Return -> ()
  | ReturnExpr e -> ()
  | Break -> ()
  | Continue -> ()
  | Block b -> ()
  | While (e, s) -> ()
  | For ((e1, e2, e3), s) -> ()
  | If (e, s) -> ()
  | IfElse (e, s1, s2) -> ()

(* TODO: ignoring params because of one function *)
let eval_func (return, id, params, block) table = eval_statement block table

(* TODO: support multiple functions *)
let rec eval_prog prog table = match prog with
  | [] -> ()
  | h :: _ -> eval_func h table

let _ =
  let prog = Lexing.from_channel stdin |> Parser.prog Lexer.read in
  eval_prog prog @@ Hashtbl.create hash_size
