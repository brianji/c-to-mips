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

(* Get list of globals and functions that elmt can see out of the available
   functions in prog. *)
let rec prog_scope elmt prog = match prog with
  | [] -> failwith "Reached end of program scope without finding function."
  | h :: t -> if h = elmt then [h] else h :: prog_scope elmt t

(* Functions for evaluating expressions. *)
let rec eval_expr expr prog vars = match expr with
  | Empty -> failwith "Empty expression." (* TODO: for loop empty expression *)
  | Var v -> eval_var v prog vars
  | Value v -> eval_value v
  | Paren e -> eval_expr e prog vars
  | FunctionCall f -> eval_function_call f prog vars
  | Infix i -> eval_infix i prog vars
  | Assign a -> eval_assign a prog vars
  | Prefix p -> eval_prefix p prog vars
  | Postfix p -> eval_postfix p prog vars
and eval_int_expr expr prog vars = match eval_expr expr prog vars with
  | Some i -> i
  | None -> failwith "Expected integer value."
and eval_var var prog vars = match vars with
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
and eval_function_call (id, args) prog vars =
  try
    (* Get function to be called *)
    let find_function f = match f with
      | Func (_, f, _, _) -> f = id
      | _ -> false
    in
    let fcn = List.find find_function prog in
    let (r, _, p, b) = match fcn with
      | Func f -> f
      | _ -> failwith "Found global instead of function."
    in

    (* New scope for function *)
    let rec get_globals = function
      | [] -> failwith "Global scope missing."
      | [t] -> t
      | _ :: t -> get_globals t
    in
    let fcn_prog = prog_scope fcn prog in
    let arg_vars = Hashtbl.create hash_size in
    let process_arg arg (_, var) =
      let value = eval_int_expr arg prog vars in
      Hashtbl.add arg_vars var (Some value)
    in
    let () = List.iter2 process_arg args p in
    let fcn_vars = [arg_vars; get_globals vars] in

    (* Evaluate function and process return *)
    match eval_statement b fcn_prog fcn_vars with
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
and eval_infix (e1, op, e2) prog vars =
  let eval e = eval_int_expr e prog vars in
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
    | Comma -> let _ = eval_expr e1 prog vars in eval e2
  in
  Some num
and eval_assign (id, op, e) prog vars =
  try
    let rhs = eval_int_expr e prog vars in
    let eval id = eval_int_expr (Var id) prog vars in
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
    let table = List.find (fun a -> Hashtbl.mem a id) vars in
    let () = Hashtbl.replace table id (Some num) in
    Some num
  with Not_found -> failwith @@ id ^ " not declared."
and eval_prefix (op, e) prog vars =
  let v = eval_int_expr e prog vars in
  let num = match op with
    | Incrmt -> let () = eval_incr e prog vars in v + 1
    | Decrmt -> let () = eval_decr e prog vars in v - 1
    | Not -> bool_of_int v |> not |> int_of_bool
    | Comp -> lnot v
    | Pos -> v
    | Neg -> -v
  in
  Some num
and eval_postfix (e, op) prog vars =
  let v = eval_int_expr e prog vars in
  let () = match op with
    | Incrmt -> eval_incr e prog vars
    | Decrmt -> eval_decr e prog vars
    | _ -> failwith "Invalid postfix operator."
  in
  Some v
and eval_incr e prog vars = match e with
  | Var v as var ->
    let next = eval_int_expr var prog vars + 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) vars in
    Hashtbl.replace table v (Some next)
  | _ -> failwith "Increment requires variable."
and eval_decr e prog vars = match e with
  | Var v as var ->
    let next = eval_int_expr var prog vars - 1 in
    let table = List.find (fun a -> Hashtbl.mem a v) vars in
    Hashtbl.replace table v (Some next)
  | _ -> failwith "Decrement requires variable."

(* Evaluates statements. *)
and eval_statements statements prog vars = match statements with
  | [] -> VoidRes
  | h :: t -> match eval_statement h prog vars with
    | VoidRes -> eval_statements t prog vars
    | other -> other
and eval_statement statement prog vars = match statement with
  | Dec (p, decs) -> eval_dec decs prog vars
  | Expr e -> let _ = eval_expr e prog vars in VoidRes
  | Return -> RetRes None
  | ReturnExpr e -> RetRes (eval_expr e prog vars)
  | Break -> BrkRes
  | Continue -> ContRes
  | Block b -> eval_statements b prog (new_scope vars)
  | While w -> eval_while w prog (new_scope vars)
  | For ((e1, e2, e3), s) ->
    let _ = eval_expr e1 prog vars in
    eval_for e2 e3 s prog (new_scope vars)
  | If i -> eval_if i prog (new_scope vars)
  | IfElse i -> eval_if_else i prog (new_scope vars)
and eval_dec decs prog vars = match decs with
  | [] -> VoidRes
  | h :: t ->
    let table = match vars with
      | [] -> failwith "Scope empty."
      | h :: _ -> h
    in
    let var, value = match h with
      | Var v ->
        if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
        else v, None
      | Assign (v, Asgmt, decs) ->
        if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
        else v, Some (eval_int_expr decs prog vars)
      | _ -> failwith "Invalid declaration expression."
    in
    let () = Hashtbl.add table var value in
    eval_dec t prog vars
and eval_while (cond, statement) prog vars =
  if eval_int_expr cond prog vars != 0 then
    match eval_statement statement prog vars with
    | VoidRes -> eval_while (cond, statement) prog vars
    | ContRes -> eval_while (cond, statement) prog vars
    | BrkRes -> VoidRes
    | other -> other
  else
    VoidRes
and eval_for cond inc statement prog vars =
  if eval_int_expr cond prog vars != 0 then
    match eval_statement statement prog vars  with
    | VoidRes ->
      let _ = eval_expr inc prog vars in
      eval_for cond inc statement prog vars
    | ContRes ->
      let _ = eval_expr inc prog vars in
      eval_for cond inc statement prog vars
    | BrkRes -> VoidRes
    | other -> other
  else
    VoidRes
and eval_if (cond, statement) prog vars =
  if eval_int_expr cond prog vars != 0 then
    eval_statement statement prog vars
  else
    VoidRes
and eval_if_else (cond, s1, s2) prog vars =
  let block = if eval_int_expr cond prog vars != 0 then s1 else s2 in
  eval_statement block prog vars

(* Process global variables *)
let rec global_scope prog elmts vars = match elmts with
  | [] -> ()
  | h :: t -> match h with
    | Global (p, decs) as g ->
      let global_prog = prog_scope g prog in
      let _ = eval_dec decs global_prog vars in
      global_scope prog t vars
    | _ -> global_scope prog t vars

let eval_main prog =
  let vars = [Hashtbl.create hash_size] in
  let () = global_scope prog prog vars in
  match eval_function_call ("main", []) prog vars with
  | Some i -> i
  | _ -> 0

(* Evaluates abstract syntax tree generated by lexing and parsing input. *)
let _ = Lexing.from_channel stdin |> Parser.prog Lexer.read |> eval_main |> exit
