.PHONY: all test clean

SRC = -I src -I test -I yacc -I camlp5
PKGS = -pkgs "GT.syntax.all,ocanren.syntax,oUnit,ocanren"
CFLGS = -cflags "-g,-rectypes"
LFLGS = -lflags "-g"

OCB_FLAGS = -use-ocamlfind -use-menhir -syntax camlp5o $(CFLGS) $(LFLGS) $(PKGS) $(SRC)
OCB = ocamlbuild $(OCB_FLAGS)

all:
	$(OCB) Main.byte
	$(OCB) Main.native

clean:
	rm -rf ./_build *.byte

test:
	$(OCB) Test.byte
	./Test.byte

FUCK = ocamlbuild -use-ocamlfind -plugin-tag "package(str)" -classic-display

plugin:
	$(FUCK) $(OCB_FLAGS) camlp5/pa_cppmem.cmo

NAME=cppmem

shit:
	# camlp5o -I . pr_o.cmo _build/camlp5/pa_$(NAME).cmo shit.ml -o shit.ppo
	# camlp5o -I . pr_r.cmo _build/camlp5/pa_$(NAME).cmo shit.ml -o shit.ppr
	$(OCB) -I . shit.byte
	# ocamlfind opt -I src -rectypes -o sort -syntax camlp5o -package ocanren,ocanren.syntax,GT -linkpkg shit.ml
	# ocamlopt -dtypes -o shit -pp 'camlp5o -I . _build/camlp5/pa_$(NAME).cmo' shit.ml
