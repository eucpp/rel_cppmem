PKGNAME=relcppmem

MKDIR ?= mkdir -vp
CP    ?= cp

SRC = -I src -I test -I yacc
PKGS = -pkgs "GT.syntax.all,ocanren.syntax,oUnit,ocanren"
CFLGS = -cflags "-g"
LFLGS = -lflags "-g"

OCB_FLAGS = -use-ocamlfind -use-menhir -syntax camlp5o $(CFLGS) $(LFLGS) $(PKGS) $(SRC)
OCB = ocamlbuild $(OCB_FLAGS)

BYTE_TARGETS = relcppmem.cma
NATIVE_TARGETS = relcppmem.cmxa

.PHONY: all test clean install uninstall

all: relcppmem plugin

clean:
	rm -rf ./_build *.byte *.native

relcppmem:
	$(OCB) $(OCB_FLAGS) $(BYTE_TARGETS) $(NATIVE_TARGETS)

plugin: relcppmem
	$(OCB) $(OCB_FLAGS) -I camlp5 camlp5/pa_cppmem.cmo

test:
	$(OCB) Test.byte
	./Test.byte

shit:
	# camlp5o -I . pr_o.cmo _build/camlp5/pa_$(NAME).cmo shit.ml -o shit.ppo
	# camlp5o -I . pr_r.cmo _build/camlp5/pa_$(NAME).cmo shit.ml -o shit.ppr
	$(OCB) -I . shit.byte
	# ocamlfind opt -I src -rectypes -o sort -syntax camlp5o -package ocanren,ocanren.syntax,GT -linkpkg shit.ml
	# ocamlopt -dtypes -o shit -pp 'camlp5o -I . _build/camlp5/pa_$(NAME).cmo' shit.ml

######################## Installation related stuff ##########################
INSTALL_TARGETS = META \
	_build/relcppmem.cmo \
	_build/relcppmem.cmx \
	_build/relcppmem.cmi \
	_build/relcppmem.o \
	_build/camlp5/pa_cppmem.cmo \

BUNDLEDIR = _build/bundle/$(PKGNAME)

define MAKE_BUNDLE_RULE
$(BUNDLEDIR)/$(notdir $(1)): $(1)
	cp $(1) $(BUNDLEDIR)
MAKE_BUNDLE_TARGETS += $(BUNDLEDIR)/$(notdir $(1))

endef
$(foreach i,$(INSTALL_TARGETS),$(eval $(call MAKE_BUNDLE_RULE,$(i)) ) )

rmbundledir:
	@$(RM) -r $(BUNDLEDIR)

$(BUNDLEDIR):
	@$(MKDIR) $@

bundle: rmbundledir $(BUNDLEDIR) $(MAKE_BUNDLE_TARGETS)

install: bundle
	ocamlfind install $(PKGNAME) $(BUNDLEDIR)/*

uninstall:
	ocamlfind remove $(PKGNAME)
