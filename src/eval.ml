open Ast

let eval_func (return, id, params, block) = ""

(* TODO: support multiple functions *)
let rec eval_prog = function
  | [] -> ""
  | h :: _ -> eval_func h

let _ =
  let s = Lexing.from_channel stdin |> Parser.prog Lexer.read |> eval_prog
  in print_string s
