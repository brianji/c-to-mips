# Process C with OCaml

Utilities for formatting and evaluating parsed C code. Written in OCaml.

[![Build Status](https://travis-ci.org/brianji/pcwo.svg?branch=master)](https://travis-ci.org/brianji/pcwo)

## Requirements
- OCaml
- OCamllex
- OCamlyacc
- OUnit
- GNU Make

## Getting Started
### Directories
#### build
Files from compiling with Makefile.
#### src
Source files for project:
- ast.ml - Abstract syntax tree type
- directives.ml - Directive matching for lexer
- eval.ml - Evaluate C code
- keywords.ml - Keyword matching for lexer
- lexer.mll - C lexer with OCamllex
- operators.ml - Operator matching for lexer
- parser.mly - C parser with OCamlyacc
- pretty_print.ml - Format C code

### Compile
Running make in the root directory will compile the utilities and generate executables, pretty_print and eval, in the root directory. Other compiled files will go into the build directory.

### Execute
The utilities take input from stdin until EOF.

## Features
### Lexer / Parser
Generate Abstract Syntax tree by lexing and parsing C code.
### Pretty Print
Formats C code by parsing input and regenerating the code.
### Evaluate
Evaluates C code by parsing input and returning to the console the value that main() returns.
