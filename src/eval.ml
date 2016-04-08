open Ast

type statement_result =
  | NoRes
  | RetRes of int
  | BrkRes
  | ContRes

let hash_size = 20

let bool_of_int i = if i = 0 then false else true
let int_of_bool b = if b then 1 else 0

let rec eval_expr expr prog scope = match expr with
  | Empty -> failwith "Empty expression." (* TODO: for loop empty expression *)
  | Var v -> eval_var v scope
  | Value v -> eval_value v
  | Paren e -> eval_expr e prog scope
  | FunctionCall _ -> failwith "Function calls unsupported."
  | Infix (e1, op, e2) -> eval_infix (e1, op, e2) prog scope
  | Assign (id, op, e) -> eval_assign (id, op, e) prog scope
  | Prefix (op, e) -> eval_prefix (op, e) prog scope
  | Postfix (e, op) -> eval_postfix (e, op) prog scope
and eval_var var scope = match scope with
  | [] -> failwith @@ var ^ " not declared."
  | h :: t ->
    if Hashtbl.mem h var then
      let v = Hashtbl.find h var in
      match v with
      | None -> failwith @@ var ^ " not initialized."
      | Some s -> s
    else eval_var var t
and eval_value v = match v with (* TODO: support other types *)
  | Integer i -> i
  | Decimal d -> int_of_float d
  | Letter l -> int_of_char l
and eval_infix (e1, op, e2) prog scope =
  let lv = eval_expr e1 prog scope in (* can evaluate this ahead of time *)
  match op with
  | Plus -> lv + (eval_expr e2 prog scope)
  | Minus -> lv - (eval_expr e2 prog scope)
  | Times -> lv * (eval_expr e2 prog scope)
  | Divide -> lv / (eval_expr e2 prog scope)
  | Mod -> lv mod (eval_expr e2 prog scope)
  | ShiftLeft -> lv lsl (eval_expr e2 prog scope)
  | ShiftRight -> lv lsr (eval_expr e2 prog scope)
  | Less -> lv < (eval_expr e2 prog scope) |> int_of_bool
  | LesserEq -> lv <= (eval_expr e2 prog scope) |> int_of_bool
  | Greater -> lv > (eval_expr e2 prog scope) |> int_of_bool
  | GreaterEq -> lv >= (eval_expr e2 prog scope) |> int_of_bool
  | Equals -> lv == (eval_expr e2 prog scope) |> int_of_bool
  | NotEquals -> lv != (eval_expr e2 prog scope) |> int_of_bool
  | BitAnd -> lv land (eval_expr e2 prog scope)
  | BitXor -> lv lxor (eval_expr e2 prog scope)
  | BitOr -> lv lor (eval_expr e2 prog scope)
  | And ->
    (bool_of_int lv && bool_of_int (eval_expr e2 prog scope)) |> int_of_bool
  | Or ->
    (bool_of_int lv || bool_of_int (eval_expr e2 prog scope)) |> int_of_bool
  | Comma -> eval_expr e2 prog scope
and eval_assign (id, op, e) prog scope =
  try
    let rhs = eval_expr e prog scope in
    let res = match op with
      | Asgmt -> rhs
      | PlusA -> rhs + (eval_var id scope)
      | MinusA -> rhs - (eval_var id scope)
      | TimesA -> rhs * (eval_var id scope)
      | DivideA -> rhs / (eval_var id scope)
      | ModA -> rhs mod (eval_var id scope)
      | ShiftLeftA -> rhs lsl (eval_var id scope)
      | ShiftRightA -> rhs lsr (eval_var id scope)
      | BitAndA -> rhs land (eval_var id scope)
      | BitOrA -> rhs lor (eval_var id scope)
      | BitXorA -> rhs lxor (eval_var id scope)
    in
    let table = List.find (fun a -> Hashtbl.mem a id) scope
    in Hashtbl.replace table id (Some res); res
  with Not_found -> failwith @@ id ^ " not declared."
and eval_prefix (op, e) prog scope =
  let v = eval_expr e prog scope in
  match op with
  | Incrmt -> eval_incr e prog scope; v + 1
  | Decrmt -> eval_decr e prog scope; v - 1
  | Not -> not @@ bool_of_int v |> int_of_bool
  | Comp -> lnot v
  | Pos -> v
  | Neg -> -v
and eval_postfix (e, op) prog scope =
  let v = eval_expr e prog scope in
  match op with
  | Incrmt -> eval_incr e prog scope; v
  | Decrmt -> eval_decr e prog scope; v
  | _ -> failwith "Invalid postfix operator."
and eval_incr e prog scope = match e with
  | Var v ->
    let curr = eval_expr e prog scope + 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) scope in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Increment requires variable."
and eval_decr e prog scope = match e with
  | Var v ->
    let curr = eval_expr e prog scope - 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) scope in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Increment requires variable."

let rec eval_dec decs prog scope = match decs with
  | [] -> ()
  | h :: t ->
    let table = match scope with
      | [] -> failwith "Scope empty."
      | h :: _ -> h
    in
    match h with
    | Var v ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else Hashtbl.add table v None; eval_dec t prog scope
    | Assign (v, Asgmt, decs) ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else
        Hashtbl.add table v (Some (eval_expr decs prog scope));
        eval_dec t prog scope
    | _ -> failwith "Invalid declaration expression."

let rec eval_statements statements prog scope = match statements with
  | [] -> NoRes
  | h :: t -> match eval_statement h prog scope with
    | NoRes -> eval_statements t prog scope
    | other -> other
and eval_statement statement prog scope = match statement with
  | Dec (p, decs) -> let () = eval_dec decs prog scope in NoRes
  | Expr e -> let _ = eval_expr e prog scope in NoRes
  | Return -> NoRes
  | ReturnExpr e -> RetRes (eval_expr e prog scope)
  | Break -> BrkRes
  | Continue -> ContRes
  | Block b -> eval_statements b prog scope
  | While (e, s) ->
    eval_while (e, s) prog (Hashtbl.create hash_size :: scope)
  | For ((e1, e2, e3), s) ->
    let new_scope = Hashtbl.create hash_size :: scope in
    let _ = eval_expr e1 new_scope in
    eval_for e2 e3 s prog new_scope
  | If (e, s) -> eval_if (e, s) prog scope
  | IfElse (e, s1, s2) -> eval_if_else (e, s1, s2) prog scope
and eval_while (cond, statement) prog scope =
  if eval_expr cond prog scope != 0 then
    match eval_statement statement prog scope with
    | NoRes -> eval_while (cond, statement) prog scope
    | ContRes -> eval_while (cond, statement) prog scope
    | BrkRes -> NoRes
    | other -> other
  else
    NoRes
and eval_for cond inc statement prog scope =
  if eval_expr cond prog scope != 0 then
    match eval_statement statement prog scope  with
    | NoRes ->
      let _ = eval_expr inc prog scope in
      eval_for cond inc statement prog scope
    | ContRes ->
      let _ = eval_expr inc prog scope in
      eval_for cond inc statement prog scope
    | BrkRes -> NoRes
    | other -> other
  else
    NoRes
and eval_if (cond, statement) prog scope =
  let new_scope = Hashtbl.create hash_size :: scope in
  if eval_expr cond prog scope != 0 then
    eval_statement statement prog new_scope
  else
    NoRes
and eval_if_else (cond, s1, s2) prog scope =
  let new_scope = Hashtbl.create hash_size :: scope in
  let block = if eval_expr cond prog scope != 0 then s1 else s2 in
  eval_statement block prog new_scope

(* TODO: process return *)
let eval_func (_, id, params, block) prog args =
  eval_statement block prog [Hashtbl.create hash_size]

let rec eval_prog prog =
  let find_main = function
    | Global _ -> false
    | Func (_, id, _, _) -> id = "main"
  in
  try 
  match List.find find_main prog with
  | Func f -> eval_func f prog []
  | Global _ -> failwith "Global variable is not main function."
  with Not_found -> failwith "main function not found."

let _ =
  match Lexing.from_channel stdin |> Parser.prog Lexer.read |> eval_prog with
  | NoRes -> print_string "No return.\n"
  | RetRes v -> print_string @@ string_of_int v ^ "\n"
  | _ -> failwith "Break or continue not inside loop or switch."
