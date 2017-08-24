PKGNAME=relcppmem

MKDIR ?= mkdir -vp
CP    ?= cp

SRC = -I src -I yacc
PKGS = -pkgs "GT.syntax.all,ocanren.syntax,ocanren"
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
	$(OCB) $(BYTE_TARGETS) $(NATIVE_TARGETS)

plugin: relcppmem
	$(OCB) -I camlp5 camlp5/pa_cppmem.cmo

test: relcppmem plugin
	$(OCB) -I test -pkgs "oUnit" Test.native
	./Test.native

################################# Samples ####################################

mp_eval : relcppmem plugin
	$(OCB) -I samples mp_eval.native
	time ./mp_eval.native

mp_synthesis: relcppmem plugin
	$(OCB) -I samples mp_synthesis.native
	time ./mp_synthesis.native

mp_sc_synthesis: relcppmem plugin
	$(OCB) -I samples mp_sc_synthesis.native
	time ./mp_sc_synthesis.native

mutex_synthesis: relcppmem plugin
	$(OCB) -I samples mutex_synthesis.native
	time ./mutex_synthesis.native

######################## Installation related stuff ##########################
INSTALL_TARGETS = META \
	_build/relcppmem.cmi \
	_build/relcppmem.cmo \
	_build/relcppmem.cmx \
	_build/relcppmem.cma \
	_build/relcppmem.cmxa \
	_build/relcppmem.o \
	_build/relcppmem.a \
	_build/camlp5/pa_cppmem.cmi \
	_build/camlp5/pa_cppmem.cmo

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
