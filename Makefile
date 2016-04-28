CC = ocamlc
PROGS = eval pretty_print

all: $(PROGS)

clean:
	@echo "Cleaning object files and executables"
	-@rm pretty_print eval *.cmo *.cmi lexer.ml parser.ml parser.mli
	cd test && make clean

test: all
	cd test && make && ./test_parser

eval: eval.cmo
	$(CC) -o eval operators.cmo directives.cmo keywords.cmo \
	  lexer.cmo parser.cmo eval.cmo ast.cmo

eval.cmo: eval.ml ast.cmo lexer.cmo parser.cmo
	$(CC) -o eval -c eval.ml

pretty_print: pretty_print.cmo
	$(CC) -o pretty_print operators.cmo directives.cmo keywords.cmo \
	  lexer.cmo parser.cmo pretty_print.cmo ast.cmo

pretty_print.cmo: pretty_print.ml ast.cmo lexer.cmo parser.cmo
	$(CC) -o pretty_print -c pretty_print.ml

ast.cmo: ast.ml
	$(CC) -o ast -c ast.ml

lexer.cmo: lexer.ml parser.cmo operators.cmo directives.cmo keywords.cmo
	$(CC) -o lexer -c lexer.ml

lexer.ml: lexer.mll
	ocamllex -o lexer.ml lexer.mll

operators.cmo: operators.ml parser.cmo
	$(CC) -o operators -c operators.ml

directives.cmo: directives.ml parser.cmo
	$(CC) -o directives -c directives.ml

keywords.cmo: keywords.ml parser.cmo
	$(CC) -o keywords -c keywords.ml

parser.cmo: parser.ml ast.cmo parser.cmi
	$(CC) -o parser -c parser.ml

parser.cmi: parser.ml ast.cmo
	$(CC) -o parser -c parser.mli

parser.ml: parser.mly
	ocamlyacc -b parser parser.mly # generates parse.ml and parser.mli
