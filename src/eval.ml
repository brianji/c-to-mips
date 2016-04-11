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

let rec prog_scope fcn prog = match prog with
  | [] -> failwith "Reached end of program scope without finding function."
  | h :: t -> if h = fcn then [h] else h :: prog_scope fcn t

let rec eval_expr expr prog local = match expr with
  | Empty -> failwith "Empty expression." (* TODO: for loop empty expression *)
  | Var v -> IntRes (eval_var v prog local)
  | Value v -> eval_value v
  | Paren e -> eval_expr e prog local
  | FunctionCall f -> eval_function_call f prog local
  | Infix i -> IntRes (eval_infix i prog local)
  | Assign a -> IntRes (eval_assign a prog local)
  | Prefix p -> IntRes (eval_prefix p prog local)
  | Postfix p -> IntRes (eval_postfix p prog local)
and eval_int_expr expr prog local = match eval_expr expr prog local with
  | IntRes i -> i
  | VoidRes -> failwith @@ "Expression requires integer operand(s)."
and eval_var var prog local = match local with
  | [] -> failwith @@ var ^ " not declared."
  | h :: t ->
    if Hashtbl.mem h var then
      match Hashtbl.find h var with
      | None -> failwith @@ var ^ " not initialized."
      | Some s -> s
    else eval_var var prog t
and eval_value v = match v with (* TODO: support other types *)
  | IntVal i -> IntRes i
  | FloatVal d -> IntRes (int_of_float d)
  | CharVal l -> IntRes (int_of_char l)
and eval_function_call (id, args) prog local =
  try
    let fcn = List.find (fun (_, f, _, _) -> f = id) prog in
    let (return, params) = match fcn with (r, _, p, _) -> (r, p) in
    let fcn_prog = prog_scope fcn prog in
    let table = Hashtbl.create hash_size in
    let process_arg arg (_, var) =
      let value = eval_int_expr arg prog local in
      Hashtbl.add table var (Some value)
    in
    let () = List.iter2 process_arg args params in
    match eval_func fcn fcn_prog [table] with
    | RetRes (IntRes i) -> (match return with
        | Void -> failwith @@ "Void function returned value."
        | _ -> IntRes i)
    | RetRes (VoidRes) -> (match return with
        | Void -> VoidRes
        | _ -> failwith @@ "Non-void function did not return value.")
    | NoRes -> VoidRes
    | BrkRes -> failwith "Break not inside loop or switch."
    | ContRes -> failwith "Continue not inside loop or switch."
  with
  | Not_found -> failwith @@ id ^ " function not found."
  | Invalid_argument _ -> failwith @@ id ^ " invalid number of arguments."
and eval_infix (e1, op, e2) prog local =
  let eval e = eval_int_expr e prog local in
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
  | Comma -> let _ = eval_expr e1 prog local in eval e2
and eval_assign (id, op, e) prog local =
  try
    let rhs = eval_int_expr e prog local in
    let eval id = eval_var id prog local in
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
and eval_prefix (op, e) prog local =
  let v = eval_int_expr e prog local in
  match op with
  | Incrmt -> let () = eval_incr e prog local in v + 1
  | Decrmt -> let () = eval_decr e prog local in v - 1
  | Not -> not @@ bool_of_int v |> int_of_bool
  | Comp -> lnot v
  | Pos -> v
  | Neg -> -v
and eval_postfix (e, op) prog local =
  let v = eval_int_expr e prog local in
  match op with
  | Incrmt -> let () = eval_incr e prog local in v
  | Decrmt -> let () = eval_decr e prog local in v
  | _ -> failwith "Invalid postfix operator."
and eval_incr e prog local = match e with
  | Var v ->
    let curr = eval_var v prog local + 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) local in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Increment requires variable."
and eval_decr e prog local = match e with
  | Var v ->
    let curr = eval_var v prog local - 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) local in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Decrement requires variable."

and eval_dec decs prog local = match decs with
  | [] -> NoRes
  | h :: t ->
    let table = match local with
      | [] -> failwith "Scope empty."
      | h :: _ -> h
    in
    match h with
    | Var v ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else let () = Hashtbl.add table v None in eval_dec t prog local
    | Assign (v, Asgmt, decs) ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else
        let () = Hashtbl.add table v (Some (eval_int_expr decs prog local)) in
        eval_dec t prog local
    | _ -> failwith "Invalid declaration expression."

and eval_statements statements prog local = match statements with
  | [] -> NoRes
  | h :: t -> match eval_statement h prog local with
    | NoRes -> eval_statements t prog local
    | other -> other
and eval_statement statement prog local = match statement with
  | Dec (p, decs) -> eval_dec decs prog local
  | Expr e -> let _ = eval_expr e prog local in NoRes
  | Return -> RetRes (VoidRes)
  | ReturnExpr e -> RetRes (eval_expr e prog local)
  | Break -> BrkRes
  | Continue -> ContRes
  | Block b -> eval_statements b prog local
  | While w -> eval_while w prog (Hashtbl.create hash_size :: local)
  | For ((e1, e2, e3), s) ->
    let new_local = Hashtbl.create hash_size :: local in
    let _ = eval_expr e1 prog new_local in
    eval_for e2 e3 s prog new_local
  | If i -> eval_if i prog local
  | IfElse i -> eval_if_else i prog local
and eval_while (cond, statement) prog local =
  if eval_int_expr cond prog local != 0 then
    match eval_statement statement prog local with
    | NoRes -> eval_while (cond, statement) prog local
    | ContRes -> eval_while (cond, statement) prog local
    | BrkRes -> NoRes
    | other -> other
  else
    NoRes
and eval_for cond inc statement prog local =
  if eval_int_expr cond prog local != 0 then
    match eval_statement statement prog local  with
    | NoRes ->
      let _ = eval_expr inc prog local in
      eval_for cond inc statement prog local
    | ContRes ->
      let _ = eval_expr inc prog local in
      eval_for cond inc statement prog local
    | BrkRes -> NoRes
    | other -> other
  else
    NoRes
and eval_if (cond, statement) prog local =
  let new_local = Hashtbl.create hash_size :: local in
  if eval_int_expr cond prog local != 0 then
    eval_statement statement prog new_local
  else
    NoRes
and eval_if_else (cond, s1, s2) prog local =
  let new_local = Hashtbl.create hash_size :: local in
  let block = if eval_int_expr cond prog local != 0 then s1 else s2 in
  eval_statement block prog new_local

(* TODO: process return type *)
and eval_func (_, _, _, block) prog local = eval_statement block prog local

let rec eval_prog prog =
  (* TODO: process prog variables *)
  let get_fcns h a = match h with
    | Func f -> f :: a
    | Global _ -> a
  in
  let fcns = List.fold_right get_fcns prog [] in
  try
    let main = List.find (fun (_, id, _, _) -> id = "main") fcns in
    let main_global = prog_scope main fcns in
    eval_func main main_global [Hashtbl.create hash_size]
  with Not_found -> failwith "main function not found."

let _ =
  match Lexing.from_channel stdin |> Parser.prog Lexer.read |> eval_prog with
  | RetRes (IntRes i) -> print_string @@ string_of_int i ^ "\n"
  | RetRes (VoidRes) -> failwith "Main did not return integer."
  | NoRes -> print_string "No result.\n"
  | BrkRes -> failwith "Break not inside loop or switch."
  | ContRes -> failwith "Continue not inside loop or switch."
