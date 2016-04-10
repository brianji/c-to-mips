open Ast

type expr_result =
  | VoidRes
  | IntRes of int
and statement_result =
  | NoRes
  | RetRes of expr_result
  | BrkRes
  | ContRes

let hash_size = 20

let bool_of_int i = if i = 0 then false else true
let int_of_bool b = if b then 1 else 0

let rec global_scope fcn global = match global with
  | [] -> failwith "Reached end of global scope without finding function."
  | h :: t -> if h = fcn then [h] else h :: global_scope fcn t

let rec eval_expr expr global local = match expr with
  | Empty -> failwith "Empty expression." (* TODO: for loop empty expression *)
  | Var v -> IntRes (eval_var v global local)
  | Value v -> eval_value v
  | Paren e -> eval_expr e global local
  | FunctionCall f -> eval_function_call f global local
  | Infix i -> IntRes (eval_infix i global local)
  | Assign a -> IntRes (eval_assign a global local)
  | Prefix p -> IntRes (eval_prefix p global local)
  | Postfix p -> IntRes (eval_postfix p global local)
and eval_int_expr expr global local = match eval_expr expr global local with
  | IntRes i -> i
  | VoidRes -> failwith @@ "Expression requires integer operand(s)."
and eval_var var global local = match local with
  | [] -> failwith @@ var ^ " not declared."
  | h :: t ->
    if Hashtbl.mem h var then
      match Hashtbl.find h var with
      | None -> failwith @@ var ^ " not initialized."
      | Some s -> s
    else eval_var var global t
and eval_value v = match v with (* TODO: support other types *)
  | Integer i -> IntRes i
  | Decimal d -> IntRes (int_of_float d)
  | Letter l -> IntRes (int_of_char l)
and eval_function_call (id, args) global local =
  try
    let fcn = List.find (fun (_, f, _, _) -> f = id) global in
    let (return, params) = match fcn with (r, _, p, _) -> (r, p) in
    let fcn_global = global_scope fcn global in
    let table = Hashtbl.create hash_size in
    let process_arg arg (_, var) =
      let value = eval_int_expr arg global local in
      Hashtbl.add table var (Some value)
    in
    let () = List.iter2 process_arg args params in
    match eval_func fcn fcn_global [table] with
    | RetRes (IntRes i) -> (match return with
        | Void -> failwith @@ "Void function returned value."
        | _ -> IntRes i)
    | RetRes (VoidRes) -> (match return with
        | Void -> VoidRes
        | _ -> failwith @@ "Non-void function did not return value.")
    | NoRes -> VoidRes
    | _ -> failwith "Break or continue not inside loop or switch."
  with
  | Not_found -> failwith @@ id ^ " function not found."
  | Invalid_argument _ -> failwith @@ id ^ " invalid number of arguments."
and eval_infix (e1, op, e2) global local =
  let eval e = eval_int_expr e global local in
  match op with
  | Plus -> (eval e1) + (eval e2)
  | Minus -> (eval e1) - (eval e2)
  | Times -> (eval e1) * (eval e2)
  | Divide -> (eval e1) / (eval e2)
  | Mod -> (eval e1) mod (eval e2)
  | ShiftLeft -> (eval e1) lsl (eval e2)
  | ShiftRight -> (eval e1) lsr (eval e2)
  | Less -> (eval e1) < (eval e2) |> int_of_bool
  | LesserEq -> (eval e1) <= (eval e2) |> int_of_bool
  | Greater -> (eval e1) > (eval e2) |> int_of_bool
  | GreaterEq -> (eval e1) >= (eval e2) |> int_of_bool
  | Equals -> (eval e1) == (eval e2) |> int_of_bool
  | NotEquals -> (eval e1) != (eval e2) |> int_of_bool
  | BitAnd -> (eval e1) land (eval e2)
  | BitXor -> (eval e1) lxor (eval e2)
  | BitOr -> (eval e1) lor (eval e2)
  | And ->
    (bool_of_int (eval e1) && bool_of_int (eval e2)) |> int_of_bool
  | Or ->
    (bool_of_int (eval e1) || bool_of_int (eval e2)) |> int_of_bool
  | Comma -> let _ = eval_expr e1 global local in eval e2
and eval_assign (id, op, e) global local =
  try
    let rhs = eval_int_expr e global local in
    let eval id = eval_var id global local in
    let res = match op with
      | Asgmt -> rhs
      | PlusA -> rhs + (eval id)
      | MinusA -> rhs - (eval id)
      | TimesA -> rhs * (eval id)
      | DivideA -> rhs / (eval id)
      | ModA -> rhs mod (eval id)
      | ShiftLeftA -> rhs lsl (eval id)
      | ShiftRightA -> rhs lsr (eval id)
      | BitAndA -> rhs land (eval id)
      | BitOrA -> rhs lor (eval id)
      | BitXorA -> rhs lxor (eval id)
    in
    let table = List.find (fun a -> Hashtbl.mem a id) local in
    let () = Hashtbl.replace table id (Some res) in
    res
  with Not_found -> failwith @@ id ^ " not declared."
and eval_prefix (op, e) global local =
  let v = eval_int_expr e global local in
  match op with
  | Incrmt -> let () = eval_incr e global local in v + 1
  | Decrmt -> let () = eval_decr e global local in v - 1
  | Not -> not @@ bool_of_int v |> int_of_bool
  | Comp -> lnot v
  | Pos -> v
  | Neg -> -v
and eval_postfix (e, op) global local =
  let v = eval_int_expr e global local in
  match op with
  | Incrmt -> let () = eval_incr e global local in v
  | Decrmt -> let () = eval_decr e global local in v
  | _ -> failwith "Invalid postfix operator."
and eval_incr e global local = match e with
  | Var v ->
    let curr = eval_var v global local + 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) local in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Increment requires variable."
and eval_decr e global local = match e with
  | Var v ->
    let curr = eval_var v global local - 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) local in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Decrement requires variable."

and eval_dec decs global local = match decs with
  | [] -> NoRes
  | h :: t ->
    let table = match local with
      | [] -> failwith "Scope empty."
      | h :: _ -> h
    in
    match h with
    | Var v ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else let () = Hashtbl.add table v None in eval_dec t global local
    | Assign (v, Asgmt, decs) ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else
        let () = Hashtbl.add table v (Some (eval_int_expr decs global local)) in
        eval_dec t global local
    | _ -> failwith "Invalid declaration expression."

and eval_statements statements global local = match statements with
  | [] -> NoRes
  | h :: t -> match eval_statement h global local with
    | NoRes -> eval_statements t global local
    | other -> other
and eval_statement statement global local = match statement with
  | Dec (p, decs) -> eval_dec decs global local
  | Expr e -> let _ = eval_expr e global local in NoRes
  | Return -> RetRes (VoidRes)
  | ReturnExpr e -> RetRes (eval_expr e global local)
  | Break -> BrkRes
  | Continue -> ContRes
  | Block b -> eval_statements b global local
  | While w -> eval_while w global (Hashtbl.create hash_size :: local)
  | For ((e1, e2, e3), s) ->
    let new_local = Hashtbl.create hash_size :: local in
    let _ = eval_expr e1 global new_local in
    eval_for e2 e3 s global new_local
  | If i -> eval_if i global local
  | IfElse i -> eval_if_else i global local
and eval_while (cond, statement) global local =
  if eval_int_expr cond global local != 0 then
    match eval_statement statement global local with
    | NoRes -> eval_while (cond, statement) global local
    | ContRes -> eval_while (cond, statement) global local
    | BrkRes -> NoRes
    | other -> other
  else
    NoRes
and eval_for cond inc statement global local =
  if eval_int_expr cond global local != 0 then
    match eval_statement statement global local  with
    | NoRes ->
      let _ = eval_expr inc global local in
      eval_for cond inc statement global local
    | ContRes ->
      let _ = eval_expr inc global local in
      eval_for cond inc statement global local
    | BrkRes -> NoRes
    | other -> other
  else
    NoRes
and eval_if (cond, statement) global local =
  let new_local = Hashtbl.create hash_size :: local in
  if eval_int_expr cond global local != 0 then
    eval_statement statement global new_local
  else
    NoRes
and eval_if_else (cond, s1, s2) global local =
  let new_local = Hashtbl.create hash_size :: local in
  let block = if eval_int_expr cond global local != 0 then s1 else s2 in
  eval_statement block global new_local

(* TODO: process return type *)
and eval_func (_, _, _, block) global local = eval_statement block global local

let rec eval_prog prog =
  (* TODO: process global variables *)
  let get_global h a = match h with
    | Func f -> f :: a
    | Global _ -> a
  in
  let global = List.fold_right get_global prog [] in
  try
    let main = List.find (fun (_, id, _, _) -> id = "main") global in
    let main_global = global_scope main global in
    eval_func main main_global [Hashtbl.create hash_size]
  with Not_found -> failwith "main function not found."

let _ =
  match Lexing.from_channel stdin |> Parser.prog Lexer.read |> eval_prog with
  | RetRes (IntRes i) -> print_string @@ string_of_int i ^ "\n"
  | RetRes (VoidRes) -> failwith "Main did not return integer."
  | NoRes -> print_string "No result.\n"
  | _ -> failwith "Break or continue not inside loop or switch."
