open Ast

let hash_size = 20

let bool_of_int i = if i = 0 then false else true
let int_of_bool b = if b then 1 else 0

let rec eval_expr expr scope = match expr with
  | Empty -> failwith "Empty expression."
  | Var v -> eval_var v scope
  | Value v -> eval_value v
  | Paren e -> eval_expr e scope
  | FunctionCall _ -> failwith "Function calls unsupported."
  | Infix (e1, op, e2) -> eval_infix (e1, op, e2) scope
  | Assign (id, op, e) -> eval_assign (id, op, e) scope
  | Prefix (op, e) -> eval_prefix (op, e) scope
  | Postfix (e, op) -> eval_postfix (e, op) scope
and eval_var var scope = match scope with
  | [] -> failwith @@ var ^ " not declared."
  | h :: t ->
    if Hashtbl.mem h var then
      let v = Hashtbl.find h var in
      match v with
      | None -> failwith @@ var ^ " not initialized."
      | Some s -> s
    else eval_var var t
and eval_value v = match v with
  | Integer i -> i
  | Decimal d -> int_of_float d
  | Letter l -> int_of_char l
and eval_infix (e1, op, e2) scope =
  let v1 = eval_expr e1 scope in
  let v2 = eval_expr e2 scope in
  match op with
  | Plus -> v1 + v2
  | Minus -> v1 - v2
  | Times -> v1 * v2
  | Divide -> v1 / v2
  | Mod -> v1 mod v2
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
and eval_assign (id, op, e) scope =
  try
    let curr = match op with
      | Asgmt -> 0 (* unused *)
      | _ -> eval_var id scope in
    let rhs = eval_expr e scope in
    let res = match op with
      | Asgmt -> rhs
      | PlusA -> rhs + curr
      | MinusA -> rhs - curr
      | TimesA -> rhs * curr
      | DivideA -> rhs / curr
      | ModA -> rhs mod curr
      | ShiftLeftA -> rhs lsl curr
      | ShiftRightA -> rhs lsr curr
      | BitAndA -> rhs land curr
      | BitOrA -> rhs lor curr
      | BitXorA -> rhs lxor curr
    in
    let table = List.find (fun a -> Hashtbl.mem a id) scope
    in Hashtbl.replace table id (Some res); res
  with Not_found -> failwith @@ id ^ " not declared."
and eval_prefix (op, e) scope =
  let v = eval_expr e scope in
  match op with
  | Incrmt -> v + 1
  | Decrmt -> v - 1
  | Not -> not @@ bool_of_int v |> int_of_bool
  | Comp -> lnot v
and eval_postfix (e, op) scope =
  let v = eval_expr e scope in
  match op with
  | Incrmt -> v + 1
  | Decrmt -> v - 1
  | _ -> failwith "Invalid postfix operator."

let rec eval_dec decs scope = match decs with
  | [] -> ()
  | h :: t ->
    let table = match scope with
      | [] -> failwith "Scope empty."
      | h :: _ -> h
    in
    match h with
    | Var v ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else Hashtbl.add table v None; eval_dec t scope
    | Assign (v, Asgmt, decs) ->
      if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
      else Hashtbl.add table v (Some (eval_expr decs scope)); eval_dec t scope
    | _ -> failwith "Invalid declaration expression."

let rec eval_statements statements scope = match statements with
  | [] -> None
  | h :: t ->
    let result = eval_statement h scope in
    match result with
    | Some v -> Some v
    | None -> eval_statements t scope
and eval_statement statement scope = match statement with
  | Dec (p, decs) -> let () = eval_dec decs scope in None
  | Expr e -> let _ = eval_expr e scope in None
  | Return -> None
  | ReturnExpr e -> Some (eval_expr e scope)
  | Break -> None
  | Continue -> None
  | Block b -> eval_statements b scope
  | While (e, s) -> None
  | For ((e1, e2, e3), s) -> None
  | If (e, s) -> eval_if (e, s) scope
  | IfElse (e, s1, s2) -> None
and eval_if (cond, statement) scope =
  if eval_expr cond scope != 0 then
    eval_statement statement ((Hashtbl.create hash_size) :: scope)
  else
    None

(* TODO: ignoring params because of one function *)
let eval_func (return, id, params, block) scope = eval_statement block scope

(* TODO: support multiple functions *)
let rec eval_prog prog scope = match prog with
  | [] -> None
  | h :: _ -> eval_func h scope

let _ =
  let prog = Lexing.from_channel stdin |> Parser.prog Lexer.read in
  let return = eval_prog prog @@ [Hashtbl.create hash_size] in
  match return with
  | None -> print_string "No return.\n"
  | Some v -> print_string @@ string_of_int v ^ "\n"
