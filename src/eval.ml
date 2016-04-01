open Ast

type statement_result =
  | NoRes
  | RetRes of int
  | BrkRes
  | ContRes

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
  let lv = eval_expr e1 scope in
  match op with
  | Plus -> lv + (eval_expr e2 scope)
  | Minus -> lv - (eval_expr e2 scope)
  | Times -> lv * (eval_expr e2 scope)
  | Divide -> lv / (eval_expr e2 scope)
  | Mod -> lv mod (eval_expr e2 scope)
  | ShiftLeft -> lv lsl (eval_expr e2 scope)
  | ShiftRight -> lv lsr (eval_expr e2 scope)
  | Less -> lv < (eval_expr e2 scope) |> int_of_bool
  | LesserEq -> lv <= (eval_expr e2 scope) |> int_of_bool
  | Greater -> lv > (eval_expr e2 scope) |> int_of_bool
  | GreaterEq -> lv >= (eval_expr e2 scope) |> int_of_bool
  | Equals -> lv == (eval_expr e2 scope) |> int_of_bool
  | NotEquals -> lv != (eval_expr e2 scope) |> int_of_bool
  | BitAnd -> lv land (eval_expr e2 scope)
  | BitXor -> lv lxor (eval_expr e2 scope)
  | BitOr -> lv lor (eval_expr e2 scope)
  | And -> (bool_of_int lv && bool_of_int (eval_expr e2 scope)) |> int_of_bool
  | Or -> (bool_of_int lv || bool_of_int (eval_expr e2 scope)) |> int_of_bool
  | Comma -> (eval_expr e2 scope)
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
  | Incrmt -> eval_incr e scope; v + 1
  | Decrmt -> eval_decr e scope; v - 1
  | Not -> not @@ bool_of_int v |> int_of_bool
  | Comp -> lnot v
  | Pos -> v
  | Neg -> -v
and eval_postfix (e, op) scope =
  let v = eval_expr e scope in
  match op with
  | Incrmt -> eval_incr e scope; v
  | Decrmt -> eval_decr e scope; v
  | _ -> failwith "Invalid postfix operator."
and eval_incr e scope = match e with
  | Var v ->
    let curr = eval_expr e scope + 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) scope in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Increment requires variable."
and eval_decr e scope = match e with
  | Var v ->
    let curr = eval_expr e scope - 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) scope in
    Hashtbl.replace table v (Some curr)
  | _ -> failwith "Increment requires variable."

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

let rec eval_statements statements scope inloop = match statements with
  | [] -> NoRes
  | h :: t ->
    let result = eval_statement h scope inloop in
    match result with
    | NoRes -> eval_statements t scope inloop
    | _ -> result
and eval_statement statement scope inloop = match statement with
  | Dec (p, decs) -> let () = eval_dec decs scope in NoRes
  | Expr e -> let _ = eval_expr e scope in NoRes
  | Return -> NoRes
  | ReturnExpr e -> RetRes (eval_expr e scope)
  | Break -> BrkRes
  | Continue -> ContRes
  | Block b -> eval_statements b scope inloop
  | While (e, s) ->
    eval_while (e, s) (Hashtbl.create hash_size :: scope) inloop
  | For ((e1, e2, e3), s) ->
    let new_scope = Hashtbl.create hash_size :: scope in
    let _ = eval_expr e1 new_scope in
    eval_for e2 e3 s new_scope inloop
  | If (e, s) -> eval_if (e, s) scope inloop
  | IfElse (e, s1, s2) -> eval_if_else (e, s1, s2) scope inloop
and eval_while (cond, statement) scope inloop =
  if eval_expr cond scope != 0 then
    let res = eval_statement statement scope true in
    match res with
    | NoRes -> eval_while (cond, statement) scope inloop
    | ContRes -> eval_while (cond, statement) scope inloop
    | BrkRes -> NoRes
    | _ -> res
  else
    NoRes
and eval_for cond inc statement scope inloop  =
  if eval_expr cond scope != 0 then
    let res = eval_statement statement scope true in
    match res with
    | NoRes ->
      let _ = eval_expr inc scope in
      eval_for cond inc statement scope inloop
    | ContRes ->
      let _ = eval_expr inc scope in
      eval_for cond inc statement scope inloop
    | BrkRes -> NoRes
    | _ -> res
  else
    NoRes
and eval_if (cond, statement) scope inloop =
  let new_scope = Hashtbl.create hash_size :: scope in
  if eval_expr cond scope != 0 then
    eval_statement statement new_scope inloop
  else
    NoRes
and eval_if_else (cond, s1, s2) scope inloop =
  let new_scope = Hashtbl.create hash_size :: scope in
  let block = if eval_expr cond scope != 0 then s1 else s2 in
  eval_statement block new_scope inloop

(* TODO: ignoring params because of one function *)
let eval_func (return, id, params, block) scope =
  eval_statement block scope false

(* TODO: support multiple functions *)
let rec eval_prog prog scope = match prog with
  | [] -> NoRes
  | h :: _ -> eval_func h scope

let _ =
  let prog = Lexing.from_channel stdin |> Parser.prog Lexer.read in
  let return = eval_prog prog @@ [Hashtbl.create hash_size] in
  match return with
  | NoRes -> print_string "No return.\n"
  | RetRes v -> print_string @@ string_of_int v ^ "\n"
  | _ -> failwith "Break or continue not inside loop or switch."
