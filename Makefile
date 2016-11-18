.PHONY: all test clean

OCB_FLAGS = -cflag "-g" -lflag "-g"
OCB = ocamlbuild $(OCB_FLAGS)

all: ;

clean:
	rm -rf ./_build *.byte

test:
	$(OCB) test.byte -pkgs oUnit -I src -I test 
	./test.byte
