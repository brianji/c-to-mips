open Ast

(* Types for results of evaluating expressions and statments. *)
type result =
  | VoidRes
  | RetRes of int option
  | BrkRes
  | ContRes

let hash_size = 20 (* for hash tables in scope list *)
let new_scope scope = Hashtbl.create hash_size :: scope

(* Convert between booleans in C and OCaml. *)
let bool_of_int i = if i = 0 then false else true
let int_of_bool b = if b then 1 else 0

(* Get list of globals and functions that fcn can see out of the available
   functions in prog. *)
let rec prog_scope fcn prog = match prog with
  | [] -> failwith "Reached end of program scope without finding function."
  | h :: t -> if h = fcn then [h] else h :: prog_scope fcn t

(* Functions for evaluating expressions. *)
let rec eval_expr expr prog local = match expr with
  | Empty -> failwith "Empty expression." (* TODO: for loop empty expression *)
  | Var v -> eval_var v prog local
  | Value v -> eval_value v
  | Paren e -> eval_expr e prog local
  | FunctionCall f -> eval_function_call f prog local
  | Infix i -> eval_infix i prog local
  | Assign a -> eval_assign a prog local
  | Prefix p -> eval_prefix p prog local
  | Postfix p -> eval_postfix p prog local
and eval_int_expr expr prog local = match eval_expr expr prog local with
  | Some i -> i
  | None -> failwith "Expected integer value."
and eval_var var prog local = match local with
  | [] -> failwith @@ var ^ " not declared."
  | h :: t ->
    if Hashtbl.mem h var then
      match Hashtbl.find h var with
      | None -> failwith @@ var ^ " not initialized."
      | value -> value
    else eval_var var prog t
and eval_value v =
  (* TODO: support other types *)
  let num = match v with
    | IntVal i -> i
    | FloatVal f -> int_of_float f
    | CharVal l -> int_of_char l
  in
  Some num
and eval_function_call (id, args) prog local =
  try
    let find_function f = match f with
      | Func (_, f, _, _) -> f = id
      | _ -> false
    in
    let fcn = List.find find_function prog in
    let (r, _, p, b) = match fcn with
      | Func f -> f
      | _ -> failwith "Found global instead of function."
    in
    let fcn_prog = prog_scope fcn prog in
    let table = Hashtbl.create hash_size in
    let process_arg arg (_, var) =
      let value = eval_int_expr arg prog local in
      Hashtbl.add table var (Some value)
    in
    let () = List.iter2 process_arg args p in
    let res = eval_statement b fcn_prog [table] in
    match res with
    | RetRes i -> (match r with
        | Void -> failwith "Void function returned value."
        | _ -> i)
    | VoidRes -> (match r with
        | Void -> None
        | _ -> failwith "Non-void function did not return value.")
    | BrkRes -> failwith "Break not inside loop or switch."
    | ContRes -> failwith "Continue not inside loop or switch."
  with
  | Not_found -> failwith @@ id ^ " function not found."
  | Invalid_argument _ -> failwith @@ id ^ " invalid number of arguments."
and eval_infix (e1, op, e2) prog local =
  let eval e = eval_int_expr e prog local in
  let num = match op with
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
    | And -> (bool_of_int (eval e1) && bool_of_int (eval e2)) |> int_of_bool
    | Or -> (bool_of_int (eval e1) || bool_of_int (eval e2)) |> int_of_bool
    | Comma -> let _ = eval_expr e1 prog local in eval e2
  in
  Some num
and eval_assign (id, op, e) prog local =
  try
    let rhs = eval_int_expr e prog local in
    let eval id = eval_int_expr (Var id) prog local in
    let num = match op with
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
    let () = Hashtbl.replace table id (Some num) in
    Some num
  with Not_found -> failwith @@ id ^ " not declared."
and eval_prefix (op, e) prog local =
  let v = eval_int_expr e prog local in
  let num = match op with
    | Incrmt -> let () = eval_incr e prog local in v + 1
    | Decrmt -> let () = eval_decr e prog local in v - 1
    | Not -> bool_of_int v |> not |> int_of_bool
    | Comp -> lnot v
    | Pos -> v
    | Neg -> -v
  in
  Some num
and eval_postfix (e, op) prog local =
  let v = eval_int_expr e prog local in
  let () = match op with
    | Incrmt -> eval_incr e prog local
    | Decrmt -> eval_decr e prog local
    | _ -> failwith "Invalid postfix operator."
  in
  Some v
and eval_incr e prog local = match e with
  | Var v as var ->
    let next = eval_int_expr var prog local + 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) local in
    Hashtbl.replace table v (Some next)
  | _ -> failwith "Increment requires variable."
and eval_decr e prog local = match e with
  | Var v as var ->
    let next = eval_int_expr var prog local - 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) local in
    Hashtbl.replace table v (Some next)
  | _ -> failwith "Decrement requires variable."

(* Evaluates statements. *)
and eval_statements statements prog local = match statements with
  | [] -> VoidRes
  | h :: t -> match eval_statement h prog local with
    | VoidRes -> eval_statements t prog local
    | other -> other
and eval_statement statement prog local = match statement with
  | Dec (p, decs) -> eval_dec decs prog local
  | Expr e -> let _ = eval_expr e prog local in VoidRes
  | Return -> RetRes None
  | ReturnExpr e -> RetRes (eval_expr e prog local)
  | Break -> BrkRes
  | Continue -> ContRes
  | Block b -> eval_statements b prog (new_scope local)
  | While w -> eval_while w prog (new_scope local)
  | For ((e1, e2, e3), s) ->
    let _ = eval_expr e1 prog local in
    eval_for e2 e3 s prog (new_scope local)
  | If i -> eval_if i prog (new_scope local)
  | IfElse i -> eval_if_else i prog (new_scope local)
and eval_dec decs prog local = match decs with
  | [] -> VoidRes
  | h :: t ->
    let table = match local with
      | [] -> failwith "Scope empty."
      | h :: _ -> h
    in
    let var, value = match h with
      | Var v ->
        if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
        else v, None
      | Assign (v, Asgmt, decs) ->
        if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
        else v, Some (eval_int_expr decs prog local)
      | _ -> failwith "Invalid declaration expression."
    in
    let () = Hashtbl.add table var value in
    eval_dec t prog local
and eval_while (cond, statement) prog local =
  if eval_int_expr cond prog local != 0 then
    match eval_statement statement prog local with
    | VoidRes -> eval_while (cond, statement) prog local
    | ContRes -> eval_while (cond, statement) prog local
    | BrkRes -> VoidRes
    | other -> other
  else
    VoidRes
and eval_for cond inc statement prog local =
  if eval_int_expr cond prog local != 0 then
    match eval_statement statement prog local  with
    | VoidRes ->
      let _ = eval_expr inc prog local in
      eval_for cond inc statement prog local
    | ContRes ->
      let _ = eval_expr inc prog local in
      eval_for cond inc statement prog local
    | BrkRes -> VoidRes
    | other -> other
  else
    VoidRes
and eval_if (cond, statement) prog local =
  if eval_int_expr cond prog local != 0 then
    eval_statement statement prog local
  else
    VoidRes
and eval_if_else (cond, s1, s2) prog local =
  let block = if eval_int_expr cond prog local != 0 then s1 else s2 in
  eval_statement block prog local

let rec eval_main prog =
  match eval_function_call ("main", []) prog [] with
  | Some i -> i
  | _ -> 0

(* Evaluates abstract syntax tree generated by lexing and parsing input. *)
let _ = Lexing.from_channel stdin |> Parser.prog Lexer.read |> eval_main |> exit
