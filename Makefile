default:
	ocamllex lexer.mll       # generates lexer.ml

clean:
	-rm ubik *cmi *cmo *~ parser.mli lexer.ml parser.ml
