# Process C with OCaml

Utilities for parsing, formatting, and evaluating C code. Written in OCaml.

[![Build Status](https://travis-ci.org/brianji/pcwo.svg?branch=master)](https://travis-ci.org/brianji/pcwo)

## Requirements
- OCaml
- OCamllex
- OCamlyacc
- OUnit
- GNU Make

## Getting Started
### Directories
#### root
Source files for project:
- ast.ml - Abstract syntax tree type
- directives.ml - Directive matching for lexer
- eval.ml - Evaluate C code
- keywords.ml - Keyword matching for lexer
- lexer.mll - C lexer with OCamllex
- operators.ml - Operator matching for lexer
- parser.mly - C parser with OCamlyacc
- pretty_print.ml - Format C code

#### test
Unit tests for project using OUnit:
- input - directory of input files for testing
  - arith.c - operators
  - basic.c - function and globals
  - call.c - function call
  - dec.c - declaration
  - ifelse.c - if-else statements
- trees.ml - ASTs used in testing
- test_parser.ml - Unit tests for lexing and parsing

### Compile
Running `make` in the root directory will compile the utilities and generate executables, pretty_print and eval, in the root directory. Other generated files will be ignored by git.

### Testing
Running `make test` in the root directory will compile the utilities, tests, and then run the tests.

### Execute
The utilities take input from stdin until EOF. The executables are found in the root directory.

## Features
### Lexer / Parser
Generate Abstract Syntax tree by lexing and parsing C code.
### Pretty Print
Formats C code by parsing input and regenerating the code.
### Evaluate
Evaluates C code by parsing input and returning to the console the value that main() returns.
