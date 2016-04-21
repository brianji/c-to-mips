open OUnit
open Ast

let get_prog input = Lexing.from_channel input |> Parser.prog Lexer.read

let test_prog () = 
  let input = open_in "test.c" in
  let result = get_prog input in
  let expected = [
    Func (Void, "f", [], Block []);
    Global (Int, [Var "x"]);
    Func (Int, "main", [], Block [])
  ] in
  assert_equal result expected

let suite_eval = "Eval" >::: 
  ["prog">:: test_prog]

let _ = Printf.printf "Parse:\n"
let _ = run_test_tt_main suite_eval
