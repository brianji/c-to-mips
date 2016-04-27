open OUnit
open Ast

(* Compares AST generated from file to expected AST *)
let compare file ast =
  let input = open_in file in
  let result = Lexing.from_channel input |> Parser.prog Lexer.read in
  assert_equal result ast

(* Unit tests *)
let test_basic () = compare "input/basic.c" Trees.basic
let test_arith () = compare "input/arith.c" Trees.arith
let test_dec () = compare "input/dec.c" Trees.dec
let test_call () = compare "input/call.c" Trees.call
let test_ifelse () = compare "input/ifelse.c" Trees.ifelse

(* List of tests to run *)
let suite_eval =
  "Parse" >:::
  ["basic" >:: test_basic;
   "arith" >:: test_arith;
   "dec" >:: test_dec;
   "call" >:: test_call;
   "ifelse" >:: test_ifelse]

(* Output test results *)
let _ = Printf.printf "Parse:\n"
let _ = run_test_tt_main suite_eval
