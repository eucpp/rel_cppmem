.PHONY: test

all: rel_cppmem

clean:
	rm -rf ./_build *.byte

rel_cppmem: ;

test:
	ocamlbuild src/test.byte -pkgs oUnit -cflag "-g" -lflag "-g"
	./test.byte
