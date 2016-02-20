SOURCEDIR = src
BUILDDIR = build

default:
	ocamllex -o $(BUILDDIR)/lexer.ml $(SOURCEDIR)/lexer.mll # generates lexer.ml

clean:
	@echo "Cleaning $(BUILDDIR)"
	@rm $(BUILDDIR)/*
