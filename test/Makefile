test: test.ml
	ocamlfind ocamlc -o test -linkpkg \
	  -I ../build/ \
	  -package oUnit \
	  ../build/operators.cmo \
	  ../build/directives.cmo \
	  ../build/keywords.cmo \
	  ../build/parser.cmo \
	  ../build/lexer.cmo \
	  test.ml

clean:
	-rm *cmi *cmo test oUnit-anon.cache *~
