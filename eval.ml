open Ast

(* Types for results of evaluating expressions and statments. *)
type expr_result =
  | IntRes of int
  | FloatRes of float
and statement_result =
  | VoidRes
  | RetRes of expr_result option
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
and eval_type_expr expr prog vars = match eval_expr expr prog vars with
  | Some i -> i
  | None -> failwith "Expected integer value."
and eval_var var prog vars = match vars with
  | [] -> failwith @@ var ^ " not declared."
  | h :: t ->
    if Hashtbl.mem h var then
      Some (Hashtbl.find h var)
    else
      eval_var var prog t
and eval_value v =
  (* TODO: support other types *)
  let value =
    match v with
      | IntVal i -> IntRes i
      | FloatVal f -> FloatRes f
      | CharVal l -> IntRes (int_of_char l)
  in Some value
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
      let value = eval_type_expr arg prog vars in
      Hashtbl.add arg_vars var value
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
  let eval e = eval_type_expr e prog vars in
  let value = match op with
    | Plus -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 + i2)
        | FloatRes f1, IntRes i2 -> FloatRes (f1 +. float_of_int i2)
        | IntRes i1, FloatRes f2 -> FloatRes (float_of_int i1 +. f2)
        | FloatRes f1, FloatRes f2 -> FloatRes (f1 +. f2)
      )
    | Minus -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 - i2)
        | FloatRes f1, IntRes i2 -> FloatRes (f1 -. float_of_int i2)
        | IntRes i1, FloatRes f2 -> FloatRes (float_of_int i1 -. f2)
        | FloatRes f1, FloatRes f2 -> FloatRes (f1 -. f2)
      )
    | Times -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 * i2)
        | FloatRes f1, IntRes i2 -> FloatRes (f1 *. float_of_int i2)
        | IntRes i1, FloatRes f2 -> FloatRes (float_of_int i1 *. f2)
        | FloatRes f1, FloatRes f2 -> FloatRes (f1 *. f2)
      )
    | Divide -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 / i2)
        | FloatRes f1, IntRes i2 -> FloatRes (f1 /. float_of_int i2)
        | IntRes i1, FloatRes f2 -> FloatRes (float_of_int i1 /. f2)
        | FloatRes f1, FloatRes f2 -> FloatRes (f1 /. f2)
      )
    | Mod -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 mod i2)
        | _ -> failwith "% requires integer values."
      )
    | ShiftLeft -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 lsl i2)
        | _ -> failwith "<< requires integer values."
      )
    | ShiftRight -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 lsr i2)
        | _ -> failwith ">> requires integer values."
      )
    | Less -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 < i2 |> int_of_bool)
        | FloatRes f1, IntRes i2 -> IntRes (f1 < float_of_int i2 |> int_of_bool)
        | IntRes i1, FloatRes f2 -> IntRes (float_of_int i1 < f2 |> int_of_bool)
        | FloatRes f1, FloatRes f2 -> IntRes (f1 < f2 |> int_of_bool)
      )
    | LesserEq -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 <= i2 |> int_of_bool)
        | FloatRes f1, IntRes i2 ->
          IntRes (f1 <= float_of_int i2 |> int_of_bool)
        | IntRes i1, FloatRes f2 ->
          IntRes (float_of_int i1 <= f2 |> int_of_bool)
        | FloatRes f1, FloatRes f2 -> IntRes (f1 <= f2 |> int_of_bool)
      )
    | Greater -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 > i2 |> int_of_bool)
        | FloatRes f1, IntRes i2 -> IntRes (f1 > float_of_int i2 |> int_of_bool)
        | IntRes i1, FloatRes f2 -> IntRes (float_of_int i1 > f2 |> int_of_bool)
        | FloatRes f1, FloatRes f2 -> IntRes (f1 > f2 |> int_of_bool)
      )
    | GreaterEq -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 >= i2 |> int_of_bool)
        | FloatRes f1, IntRes i2 ->
          IntRes (f1 >= float_of_int i2 |> int_of_bool)
        | IntRes i1, FloatRes f2 ->
          IntRes (float_of_int i1 >= f2 |> int_of_bool)
        | FloatRes f1, FloatRes f2 -> IntRes (f1 >= f2 |> int_of_bool)
      )
    | Equals -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 == i2 |> int_of_bool)
        | FloatRes f1, IntRes i2 ->
          IntRes (f1 == float_of_int i2 |> int_of_bool)
        | IntRes i1, FloatRes f2 ->
          IntRes (float_of_int i1 == f2 |> int_of_bool)
        | FloatRes f1, FloatRes f2 -> IntRes (f1 == f2 |> int_of_bool)
      )
    | NotEquals -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 != i2 |> int_of_bool)
        | FloatRes f1, IntRes i2 ->
          IntRes (f1 != float_of_int i2 |> int_of_bool)
        | IntRes i1, FloatRes f2 ->
          IntRes (float_of_int i1 != f2 |> int_of_bool)
        | FloatRes f1, FloatRes f2 -> IntRes (f1 != f2 |> int_of_bool)
      )
    | BitAnd -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 land i2)
        | _ -> failwith "& requires integer values."
      )
    | BitXor -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 lxor i2)
        | _ -> failwith "^ requires integer values."
      )
    | BitOr -> (
        match eval e1, eval e2 with
        | IntRes i1, IntRes i2 -> IntRes (i1 lor i2)
        | _ -> failwith "| requires integer values."
      )
    | And -> (
        match eval e1 with
        | IntRes i1 -> (
            match eval e2 with
            | IntRes i2 ->
              IntRes ((bool_of_int i1 && bool_of_int i2) |> int_of_bool)
            | _ -> failwith "&& requires integer values."
          )
        | _ -> failwith "&& requires integer values."
      )
    | Or -> (
        match eval e1 with
        | IntRes i1 -> (
            match eval e2 with
            | IntRes i2 ->
              IntRes ((bool_of_int i1 || bool_of_int i2) |> int_of_bool)
            | _ -> failwith "|| requires integer values."
          )
        | _ -> failwith "|| requires integer values."
      )
    | Comma -> let _ = eval_expr e1 prog vars in eval e2
  in
  Some value
and eval_assign (id, op, e) prog vars =
  try
    let eval inop = eval_type_expr (Infix ((Var id), inop, e)) prog vars in
    let table = List.find (fun a -> Hashtbl.mem a id) vars in
    let value = match op with
      | Asgmt -> eval_type_expr e prog vars
      | PlusA -> eval Plus
      | MinusA -> eval Minus
      | TimesA -> eval Times
      | DivideA -> eval Divide
      | ModA -> eval Mod
      | ShiftLeftA -> eval ShiftLeft
      | ShiftRightA -> eval ShiftRight
      | BitAndA -> eval BitAnd
      | BitOrA -> eval BitOr
      | BitXorA -> eval BitXor
    in
    let res = match Hashtbl.find table id with
      | IntRes _ -> (
          match value with
          | IntRes _ -> value
          | FloatRes f -> IntRes (int_of_float f)
        )
      | FloatRes _ -> (
          match value with
          | IntRes i -> FloatRes (float_of_int i)
          | FloatRes _ -> value
        )
    in
    let () = Hashtbl.replace table id res in
    Some res
  with Not_found -> failwith @@ id ^ " not declared."
and eval_prefix (op, e) prog vars =
  let v = eval_type_expr e prog vars in
  let res = match op with
    | Incrmt ->
      let () = eval_incr e prog vars in (
        match v with
        | IntRes i -> IntRes (i + 1)
        | FloatRes f -> FloatRes (f +. 1.0)
      )
    | Decrmt ->
      let () = eval_decr e prog vars in (
        match v with
        | IntRes i -> IntRes (i - 1)
        | FloatRes f -> FloatRes (f -. 1.0)
      )
    | Not -> (
        match v with
        | IntRes i -> IntRes (bool_of_int i |> not |> int_of_bool)
        | FloatRes _ -> failwith "! requires integer value."
      )
    | Comp -> (
        match v with
        | IntRes i -> IntRes (lnot i)
        | FloatRes _ -> failwith "! requires integer value."
      )
    | Pos -> v
    | Neg -> (
        match v with
        | IntRes i -> IntRes (-i)
        | FloatRes f -> FloatRes (-.f)
      )
  in
  Some res
and eval_postfix (e, op) prog vars =
  let v = eval_type_expr e prog vars in
  let res = match op with
    | Incrmt -> let () = eval_incr e prog vars in v
    | Decrmt -> let () = eval_decr e prog vars in v
    | _ -> failwith "Invalid postfix operator."
  in
  Some res
and eval_incr e prog vars = match e with
  | Var v as var ->
    let next = match eval_type_expr var prog vars with
      | IntRes i -> IntRes (i + 1)
      | FloatRes f -> FloatRes (f +. 1.0)
    in
    let table = List.find (fun a -> Hashtbl.mem a v) vars in
    Hashtbl.replace table v next
  | _ -> failwith "Increment requires variable."
and eval_decr e prog vars = match e with
  | Var v as var ->
    let next = match eval_type_expr var prog vars with
      | IntRes i -> IntRes (i - 1)
      | FloatRes f -> FloatRes (f -. 1.0)
    in
    let table = List.find (fun a -> Hashtbl.mem a v) vars in
    Hashtbl.replace table v next
  | _ -> failwith "Decrement requires variable."

(* Evaluates statements. *)
and eval_statements statements prog vars = match statements with
  | [] -> VoidRes
  | h :: t -> match eval_statement h prog vars with
    | VoidRes -> eval_statements t prog vars
    | other -> other
and eval_statement statement prog vars = match statement with
  | Dec d -> eval_dec d prog vars
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
and eval_dec (prim, decs) prog vars = match decs with
  | [] -> VoidRes
  | h :: t ->
    let table = match vars with
      | [] -> failwith "Scope empty."
      | h :: _ -> h
    in
    let var, value = match h with
      | Var v ->
        let () = Random.self_init () in
        if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
        else v, (
            match prim with
            | Int | Char -> IntRes (Random.int max_int)
            | Float -> FloatRes (Random.float max_float)
            | Void -> failwith "Cannot declare void variables."
          )
      | Assign (v, Asgmt, expr) ->
        if Hashtbl.mem table v then failwith @@ v ^ " is already declared."
        else v, (
            let res = eval_type_expr expr prog vars in
            match prim with
            | Int | Char -> (
                match res with
                | IntRes _ -> res
                | FloatRes f -> IntRes (int_of_float f)
              )
            | Float -> (
                match res with
                | IntRes i -> FloatRes (float_of_int i)
                | FloatRes _ -> res
              )
            | Void -> failwith "Cannot declare void variables."
          )
      | _ -> failwith "Invalid declaration expression."
    in
    let () = Hashtbl.add table var value in
    eval_dec (prim, t) prog vars
and eval_while (cond, statement) prog vars =
  let res = match eval_type_expr cond prog vars with
    | IntRes i -> i
    | FloatRes _ -> failwith "While condition cannot be float value."
  in
  if res != 0 then
    match eval_statement statement prog vars with
    | VoidRes -> eval_while (cond, statement) prog vars
    | ContRes -> eval_while (cond, statement) prog vars
    | BrkRes -> VoidRes
    | other -> other
  else
    VoidRes
and eval_for cond inc statement prog vars =
  let res = match eval_type_expr cond prog vars with
    | IntRes i -> i
    | FloatRes _ -> failwith "For condition cannot be float value."
  in
  if res != 0 then
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
  let res = match eval_type_expr cond prog vars with
    | IntRes i -> i
    | FloatRes _ -> failwith "If condition cannot be float value."
  in
  if res != 0 then
    eval_statement statement prog vars
  else
    VoidRes
and eval_if_else (cond, s1, s2) prog vars =
  let res = match eval_type_expr cond prog vars with
    | IntRes i -> i
    | FloatRes _ -> failwith "If condition cannot be float value."
  in
  let block = if res != 0 then s1 else s2 in
  eval_statement block prog vars

(* Evaluate global variable declarations *)
let rec global_scope prog elmts vars = match elmts with
  | [] -> ()
  | h :: t -> match h with
    | Global g ->
      let global_prog = prog_scope h prog in
      let _ = eval_dec g global_prog vars in
      global_scope prog t vars
    | _ -> global_scope prog t vars

(* Process globals variables and evaluate main() *)
let eval prog =
  let vars = [Hashtbl.create hash_size] in
  let () = global_scope prog prog vars in
  match eval_function_call ("main", []) prog vars with
  | Some r -> (
      match r with
      | IntRes i -> i
      | FloatRes f -> int_of_float f
    )
  | _ -> 0

(* Evaluates abstract syntax tree generated by lexing and parsing input. *)
let _ = Lexing.from_channel stdin |> Parser.prog Lexer.read |> eval |> exit
