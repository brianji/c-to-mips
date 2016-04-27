ARGS = -linkpkg \
	-I .. \
	-package oUnit \
	operators.cmo \
	directives.cmo \
	keywords.cmo \
	parser.cmo \
	lexer.cmo \
	trees.cmo
PROGS = test_parser

all: test_parser

test_parser: test_parser.ml trees.cmo
	ocamlfind ocamlc -o test_parser $(ARGS) test_parser.ml

trees.cmo: trees.ml
	ocamlc -I .. -c trees.ml

clean:
	-rm *.cmi *.cmo $(PROGS) oUnit-anon.cache *~
