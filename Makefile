default:
	ocamllex lexer.mll			# generates lexer.ml
	ocamlyacc parser.mly		# generates parse.ml and parser.mli
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c operators.ml
	ocamlc -c directives.ml
	ocamlc -c keywords.ml
	ocamlc -c lexer.ml
	ocamlc -c print.ml
	ocamlc -c test.ml
	ocamlc -o test operators.cmo directives.cmo keywords.cmo lexer.cmo parser.cmo test.cmo
	ocamlc -o print operators.cmo directives.cmo keywords.cmo lexer.cmo print.cmo
clean:
	-rm test print *cmi *cmo *~ parser.mli lexer.ml parser.ml
