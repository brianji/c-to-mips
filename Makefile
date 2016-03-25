SOURCEDIR = src
BUILDDIR = build
CC = ocamlc -I $(BUILDDIR)
PROGS = pretty_print

all: $(PROGS)

clean:
	@echo "Cleaning $(BUILDDIR) and executables"
	-@rm $(BUILDDIR)/* pretty_print 2>/dev/null || true

pretty_print: pretty_print.cmo
	$(CC) -o pretty_print operators.cmo directives.cmo keywords.cmo lexer.cmo parser.cmo pretty_print.cmo ast.cmo

pretty_print.cmo: ast.cmo lexer.cmo parser.cmo
	$(CC) -o $(BUILDDIR)/pretty_print -c $(SOURCEDIR)/pretty_print.ml

ast.cmo:
	$(CC) -o $(BUILDDIR)/ast -c $(SOURCEDIR)/ast.ml

lexer.cmo: lexer.ml parser.cmo operators.cmo directives.cmo keywords.cmo
	$(CC) -o $(BUILDDIR)/lexer -c $(BUILDDIR)/lexer.ml

lexer.ml:
	ocamllex -o $(BUILDDIR)/lexer.ml $(SOURCEDIR)/lexer.mll # generates lexer.ml

operators.cmo: parser.cmo
	$(CC) -o $(BUILDDIR)/operators -c $(SOURCEDIR)/operators.ml

directives.cmo: parser.cmo
	$(CC) -o $(BUILDDIR)/directives -c $(SOURCEDIR)/directives.ml

keywords.cmo: parser.cmo
	$(CC) -o $(BUILDDIR)/keywords -c $(SOURCEDIR)/keywords.ml

parser.cmo: parser ast.cmo parser.cmi
	$(CC) -o $(BUILDDIR)/parser -c $(BUILDDIR)/parser.ml

parser.cmi: parser ast.cmo
	$(CC) -o $(BUILDDIR)/parser -c $(BUILDDIR)/parser.mli

parser:
	ocamlyacc -b $(BUILDDIR)/parser $(SOURCEDIR)/parser.mly # generates parse.ml and parser.mli
