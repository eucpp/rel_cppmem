.PHONY: all test clean

#OCB_FLAGS = -use-ocamlfind -cflag "-g" -cflag "-rectypes" -lflag "-g" -pkgs GT.syntax.all -pkgs ocanren.syntax -pkgs oUnit -pkgs ocanren -I src -I test -syntax 'camlp5' -ppflag "pr_dump.cmo"
OCB_FLAGS = -use-ocamlfind -syntax camlp5o -cflag "-g" -cflag "-rectypes" -lflag "-g" -pkgs GT.syntax.all -pkgs ocanren.syntax -I src -I test 
OCB = ocamlbuild $(OCB_FLAGS)

all: 
	$(OCB) logic.byte 

clean:
	rm -rf ./_build *.byte

test:
	$(OCB) test.byte  
	./test.byte
