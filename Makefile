.PHONY: all test clean

SRC = -I src -I test -I yacc
PKGS = -pkgs "GT.syntax.all,ocanren.syntax,oUnit,ocanren"
CFLGS = -cflags "-g,-rectypes"
LFLGS = -lflags "-g"

OCB_FLAGS = -use-ocamlfind -use-menhir -syntax camlp5o $(CFLGS) $(LFLGS) $(PKGS) $(SRC)
OCB = ocamlbuild $(OCB_FLAGS)

LEX  = ocamllex
YACC = menhir -v

all:
	$(LEX) yacc/lexer.mll
	$(YACC) yacc/parser.mly

clean:
	rm -rf ./_build *.byte

test:
	$(OCB) Test.byte
	./Test.byte
