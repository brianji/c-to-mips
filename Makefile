SOURCEDIR = src
BUILDDIR = build
CC = ocamlc -I $(BUILDDIR)

default:
	ocamllex -o $(BUILDDIR)/lexer.ml $(SOURCEDIR)/lexer.mll # generates lexer.ml
	ocamlyacc -b $(BUILDDIR)/parser $(SOURCEDIR)/parser.mly # generates parse.ml and parser.mli
	$(CC) -o $(BUILDDIR)/ast -c $(SOURCEDIR)/ast.ml
	$(CC) -o $(BUILDDIR)/parser -c $(BUILDDIR)/parser.mli
	$(CC) -o $(BUILDDIR)/parser -c $(BUILDDIR)/parser.ml
	$(CC) -o $(BUILDDIR)/operators -c $(SOURCEDIR)/operators.ml
	$(CC) -o $(BUILDDIR)/directives -c $(SOURCEDIR)/directives.ml
	$(CC) -o $(BUILDDIR)/keywords -c $(SOURCEDIR)/keywords.ml
	$(CC) -o $(BUILDDIR)/lexer -c $(BUILDDIR)/lexer.ml
	$(CC) -o $(BUILDDIR)/pretty_print -c $(SOURCEDIR)/pretty_print.ml
	$(CC) -o pretty_print operators.cmo directives.cmo keywords.cmo lexer.cmo parser.cmo pretty_print.cmo
clean:
	@echo "Cleaning $(BUILDDIR) and executables"
	@rm $(BUILDDIR)/* pretty_print print
