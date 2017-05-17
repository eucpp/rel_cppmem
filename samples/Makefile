all:
	ocamlfind opt -rectypes -syntax camlp5o -package ocanren,ocanren.syntax,GT,relcppmem,relcppmem.syntax -linkpkg -o message_passing message_passing.ml
	#ocamlbuild -use-ocamlfind -rectypes -syntax camlp5o -pkgs "GT.syntax.all,ocanren,ocanren.syntax,relcppmem,relcppmem.syntax" message_passing.native

	# ocamlfind opt -rectypes -o tree -syntax camlp5o -package ocanren,ocanren.syntax,GT,GT.syntax.all -linkpkg tree.ml

clean:
	rm *.cm[oix] *.o message_passing
