all:
	ocamlfind opt -g -rectypes -syntax camlp5o -package ocanren,ocanren.syntax,GT,relcppmem,relcppmem.syntax -linkpkg -o mp_eval mp_eval.ml

clean:
	rm *.cm[oix] *.o mp_eval
