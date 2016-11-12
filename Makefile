.PHONY: test

all: rel_cppmem

clean:
	rm -rf ./_build *.byte

rel_cppmem: ;

test:
	ocamlbuild test.byte -pkgs oUnit
	./test.byte
