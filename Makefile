default:
	ocamllex lexer.mll			# generates lexer.ml
	ocamlyacc parser.mly		# generates parse.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c operators.ml
	ocamlc -c directives.ml
	ocamlc -c keywords.ml
	ocamlc -c lexer.ml

clean:
	-rm ubik *cmi *cmo *~ parser.mli lexer.ml parser.ml
