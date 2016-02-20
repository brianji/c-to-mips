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
	$(CC) -o $(BUILDDIR)/print -c $(SOURCEDIR)/print.ml
	$(CC) -o $(BUILDDIR)/test -c $(SOURCEDIR)/test.ml
	$(CC) -o test operators.cmo directives.cmo keywords.cmo lexer.cmo parser.cmo test.cmo
	$(CC) -o print operators.cmo directives.cmo keywords.cmo lexer.cmo print.cmo
clean:
	@echo "Cleaning $(BUILDDIR) and executables"
	@rm $(BUILDDIR)/* test print
