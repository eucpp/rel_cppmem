all:
	ocamlfind opt -rectypes -syntax camlp5o -package ocanren,ocanren.syntax,GT,relcppmem,relcppmem.syntax -linkpkg -o message_passing message_passing.ml
	ocamlfind opt -g -rectypes -syntax camlp5o -package ocanren,ocanren.syntax,GT,relcppmem,relcppmem.syntax -linkpkg -o mp_synthesis mp_synthesis.ml
	ocamlfind opt -g -rectypes -syntax camlp5o -package ocanren,ocanren.syntax,GT,relcppmem,relcppmem.syntax -linkpkg -o mutex_synthesis mutex_synthesis.ml

clean:
	rm *.cm[oix] *.o mp_synthesis mutex_synthesis message_passing
