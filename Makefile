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
	ocamlc -c main.ml
	ocamlc -o main directives.cmo keywords.cmo operators.cmo lexer.cmo main.cmo

clean:
	-rm ubik *cmi *cmo *~ parser.mli lexer.ml parser.ml
