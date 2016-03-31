open Ast

let hash_size = 100

let bool_of_int i = if i = 0 then false else true
let int_of_bool b = if b then 1 else 0

let rec eval_expr expr table = match expr with
  | Empty -> failwith "Empty expression."
  | Value v -> eval_value v
  | Infix (e1, op, e2) -> eval_infix (e1, op, e2) table
  | Prefix (op, e) -> eval_prefix (op, e) table
  | Postfix (e, op) -> eval_postfix (e, op) table
  | _ -> failwith "Expression unsupported."
and eval_value v = match v with
  | Integer i -> i
  | Decimal d -> int_of_float d
  | Letter l -> int_of_char l
  (* TODO: support more than integer arithmetic *)
and eval_infix (e1, op, e2) table =
  let v1 = eval_expr e1 table in
  let v2 = eval_expr e2 table in
  match op with
  | Plus -> v1 + v2
  | Minus -> v1 - v2
  | Times -> v1 * v2
  | Divide -> v1 / v2
  | Mod -> v1 mod v2
  | Asgmt -> v2 (* TODO: incomplete *)
  | ShiftLeft -> v1 lsl v2
  | ShiftRight -> v1 lsr v2
  | Less -> v1 < v2 |> int_of_bool
  | LesserEq -> v1 <= v2 |> int_of_bool
  | Greater -> v1 > v2 |> int_of_bool
  | GreaterEq -> v1 >= v2 |> int_of_bool
  | Equals -> v1 == v2 |> int_of_bool
  | NotEquals -> v1 != v2 |> int_of_bool
  | BitAnd -> v1 land v2
  | BitXor -> v1 lxor v2
  | BitOr -> v1 lor v2
  (* TODO: short circuit *)
  | And -> (bool_of_int v1 && bool_of_int v2) |> int_of_bool
  | Or -> (bool_of_int v1 || bool_of_int v2) |> int_of_bool
  | Comma -> v2
  | _ -> failwith "Operation unsupported."
and eval_prefix (op, e) table =
  let v = eval_expr e table in
  match op with
  | Incrmt -> v + 1
  | Decrmt -> v - 1
  | Not -> not @@ bool_of_int v |> int_of_bool
  | Comp -> lnot v
and eval_postfix (e, op) table =
  let v = eval_expr e table in
  match op with
  | Incrmt -> v + 1
  | Decrmt -> v - 1
  | _ -> failwith "Invalid postfix operator."

let rec eval_statements statements table = match statements with
  | [] -> None
  | h :: t -> match h with
    | Return -> None
    | ReturnExpr e -> Some (eval_expr e table)
    | _ ->
      let _ = eval_statement h table in
       eval_statements t table
and eval_statement statement table = match statement with
  | Dec (p, decs) -> None
  | Expr e -> None
  | Return -> None
  | ReturnExpr e -> Some (eval_expr e table)
  | Break -> None
  | Continue -> None
  | Block b -> eval_statements b table
  | While (e, s) -> None
  | For ((e1, e2, e3), s) -> None
  | If (e, s) -> None
  | IfElse (e, s1, s2) -> None

(* TODO: ignoring params because of one function *)
let eval_func (return, id, params, block) table = eval_statement block table

(* TODO: support multiple functions *)
let rec eval_prog prog table = match prog with
  | [] -> None
  | h :: _ -> eval_func h table

let _ =
  let prog = Lexing.from_channel stdin |> Parser.prog Lexer.read in
  let return = eval_prog prog @@ Hashtbl.create hash_size in
  match return with
  | None -> print_string "No return."; print_newline ()
  | Some v -> print_int v; print_newline ()
