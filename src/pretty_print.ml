open Ast
open To_string

let _ =
  let s = Lexing.from_channel stdin |> Parser.main Lexer.read |> function_string
  in print_string s
