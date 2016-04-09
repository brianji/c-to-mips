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

let rec eval_expr expr fcns scope = match expr with
  | Empty -> failwith "Empty expression." (* TODO: for loop empty expression *)
  | Var v -> IntRes (eval_var v fcns scope)
  | Value v -> eval_value v
  | Paren e -> eval_expr e fcns scope
  | FunctionCall f -> eval_function_call f fcns scope
  | Infix i -> IntRes (eval_infix i fcns scope)
  | Assign a -> IntRes (eval_assign a fcns scope)
  | Prefix p -> IntRes (eval_prefix p fcns scope)
  | Postfix p -> IntRes (eval_postfix p fcns scope)
and eval_int_expr expr fcns scope = match eval_expr expr fcns scope with
  | IntRes i -> i
  | VoidRes -> failwith @@ "Expression requires integer operand(s)."
and eval_var var fcns scope = match scope with
  | [] -> failwith @@ var ^ " not declared."
  | h :: t ->
    if Hashtbl.mem h var then
      match Hashtbl.find h var with
      | None -> failwith @@ var ^ " not initialized."
      | Some s -> s
    else eval_var var fcns t
and eval_value v = match v with (* TODO: support other types *)
  | Integer i -> IntRes i
  | Decimal d -> IntRes (int_of_float d)
  | Letter l -> IntRes (int_of_char l)
and eval_function_call (id, args) fcns scope =
  try
    let fcn = List.find (fun (_, f, _, _) -> String.compare f id = 0) fcns in
    let (return, params) = match fcn with (r, _, p, _) -> (r, p) in
    let table = Hashtbl.create hash_size in
    let process_arg arg (_, var) = 
      let value = eval_int_expr arg fcns scope in
      Hashtbl.add table var (Some value)
    in
    let () = List.iter2 process_arg args params in
    match eval_func fcn fcns [table] with
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
and eval_infix (e1, op, e2) fcns scope =
  let eval e = eval_int_expr e fcns scope in
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
  | Comma -> let _ = eval_expr e1 fcns scope in eval e2
and eval_assign (id, op, e) fcns scope =
  try
    let rhs = eval_int_expr e fcns scope in
    let eval id = eval_var id fcns scope in
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
    let table = List.find (fun a -> Hashtbl.mem a id) scope in 
    let () = Hashtbl.replace table id (Some res) in
    res
  with Not_found -> failwith @@ id ^ " not declared."
and eval_prefix (op, e) fcns scope =
  let v = eval_int_expr e fcns scope in
  match op with
  | Incrmt -> let () = eval_incr e fcns scope in v + 1
  | Decrmt -> let () = eval_decr e fcns scope in v - 1
  | Not -> not @@ bool_of_int v |> int_of_bool
  | Comp -> lnot v
  | Pos -> v
  | Neg -> -v
and eval_postfix (e, op) fcns scope =
  let v = eval_int_expr e fcns scope in
  match op with
  | Incrmt -> let () = eval_incr e fcns scope in v
  | Decrmt -> let () = eval_decr e fcns scope in v
  | _ -> failwith "Invalid postfix operator."
and eval_incr e fcns scope = match e with
  | Var v ->
    let curr = eval_var v fcns scope + 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) scope in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Increment requires variable."
and eval_decr e fcns scope = match e with
  | Var v ->
    let curr = eval_var v fcns scope - 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) scope in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Decrement requires variable."

and eval_dec decs fcns scope = match decs with
  | [] -> NoRes
  | h :: t ->
    let table = match scope with
      | [] -> failwith "Scope empty."
      | h :: _ -> h
    in
    match h with
    | Var v ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else let () = Hashtbl.add table v None in eval_dec t fcns scope
    | Assign (v, Asgmt, decs) ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else
        let () = Hashtbl.add table v (Some (eval_int_expr decs fcns scope)) in
        eval_dec t fcns scope
    | _ -> failwith "Invalid declaration expression."

and eval_statements statements fcns scope = match statements with
  | [] -> NoRes
  | h :: t -> match eval_statement h fcns scope with
    | NoRes -> eval_statements t fcns scope
    | other -> other
and eval_statement statement fcns scope = match statement with
  | Dec (p, decs) -> eval_dec decs fcns scope
  | Expr e -> let _ = eval_expr e fcns scope in NoRes
  | Return -> RetRes (VoidRes)
  | ReturnExpr e -> RetRes (eval_expr e fcns scope)
  | Break -> BrkRes
  | Continue -> ContRes
  | Block b -> eval_statements b fcns scope
  | While w -> eval_while w fcns (Hashtbl.create hash_size :: scope)
  | For ((e1, e2, e3), s) ->
    let new_scope = Hashtbl.create hash_size :: scope in
    let _ = eval_expr e1 fcns new_scope in
    eval_for e2 e3 s fcns new_scope
  | If i -> eval_if i fcns scope
  | IfElse i -> eval_if_else i fcns scope
and eval_while (cond, statement) fcns scope =
  if eval_int_expr cond fcns scope != 0 then
    match eval_statement statement fcns scope with
    | NoRes -> eval_while (cond, statement) fcns scope
    | ContRes -> eval_while (cond, statement) fcns scope
    | BrkRes -> NoRes
    | other -> other
  else
    NoRes
and eval_for cond inc statement fcns scope =
  if eval_int_expr cond fcns scope != 0 then
    match eval_statement statement fcns scope  with
    | NoRes ->
      let _ = eval_expr inc fcns scope in
      eval_for cond inc statement fcns scope
    | ContRes ->
      let _ = eval_expr inc fcns scope in
      eval_for cond inc statement fcns scope
    | BrkRes -> NoRes
    | other -> other
  else
    NoRes
and eval_if (cond, statement) fcns scope =
  let new_scope = Hashtbl.create hash_size :: scope in
  if eval_int_expr cond fcns scope != 0 then
    eval_statement statement fcns new_scope
  else
    NoRes
and eval_if_else (cond, s1, s2) fcns scope =
  let new_scope = Hashtbl.create hash_size :: scope in
  let block = if eval_int_expr cond fcns scope != 0 then s1 else s2 in
  eval_statement block fcns new_scope

(* TODO: process return type *)
and eval_func (_, _, _, block) fcns scope = eval_statement block fcns scope

let rec eval_prog prog =
  (* TODO: process global variables *)
  let get_function h a = match h with
    | Func f -> f :: a
    | Global _ -> a
  in
  let fcns = List.fold_right get_function prog [] in
  try
    let main = List.find (fun (_, id, _, _) -> id = "main") fcns in
    eval_func main fcns [Hashtbl.create hash_size]
  with Not_found -> failwith "main function not found."

let _ =
  match Lexing.from_channel stdin |> Parser.prog Lexer.read |> eval_prog with
  | RetRes (IntRes i) -> print_string @@ string_of_int i ^ "\n"
  | RetRes (VoidRes) -> failwith "Main did not return integer."
  | NoRes -> print_string "No result.\n"
  | _ -> failwith "Break or continue not inside loop or switch."
