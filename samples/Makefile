PKGS = ocanren,ocanren.syntax,GT,relcppmem,relcppmem.syntax
FLAGS = -g -rectypes -syntax camlp5o -linkpkg

SAMPLES = mp_eval mp_evalo mp_ra_eval mp_ra_invariant mp_ra_reachable

all:
	for sample in $(SAMPLES); do \
    ocamlfind opt $(FLAGS) -package $(PKGS) -o $$sample "$$sample.ml" ; \
	done

clean:
	rm *.cm[oix] *.o
	for sample in $(SAMPLES); do \
    rm $$sample ; \
	done
