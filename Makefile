.PHONY: all test clean

OCB_FLAGS = -use-ocamlfind -syntax camlp5o -cflag "-g" -cflag "-rectypes" -lflag "-g" -pkgs GT.syntax.all -pkgs ocanren.syntax -pkgs oUnit -pkgs ocanren -I src -I test -I yacc
OCB = ocamlbuild $(OCB_FLAGS)

LEX  = ocamllex
YACC = ocamlyacc -v

all:
	$(LEX) yacc/lexer.mll
	$(YACC) yacc/parser.mly

clean:
	rm -rf ./_build *.byte

test:
	$(OCB) Test.byte
	./Test.byte
