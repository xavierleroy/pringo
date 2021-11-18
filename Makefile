OCAMLFLAGS=-g -safe-string -bin-annot
OCAMLC=ocamlc $(OCAMLFLAGS)
OCAMLOPT=ocamlopt $(OCAMLFLAGS)
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib
OCAMLFIND=ocamlfind
DIEHARDER=dieharder -g 200 -a
ENT=head -c 1000000 | ent

include $(shell $(OCAMLC) -where)/Makefile.config

all: PRNG.cmxa PRNG.cma

PRNG.cmxa PRNG.cma: PRNG.cmx PRNG.cmo stubs.$(O)
	$(OCAMLMKLIB) -o PRNG PRNG.cmo PRNG.cmx stubs.$(O)

test/u01.exe: test/u01.ml PRNG.cmxa
	ocamlfind ocamlopt -package testu01 -linkpkg $(OCAMLFLAGS) -I . \
          -o test/u01.exe \
          PRNG.cmxa test/u01.ml

%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml
%.cmo: %.ml
	$(OCAMLC) -c $*.ml
%.cmi: %.mli
	$(OCAMLOPT) -c $*.mli
%.$(O): %.c
	$(OCAMLC) -c $*.c
%.exe: %.ml PRNG.cmxa
	$(OCAMLOPT) -I . -o $@ PRNG.cmxa $*.ml

clean::
	rm -f *.cm[ioxa] *.cmxa *.$(O) *.$(A) *.$(SO)
	rm -f test/*.cm[iox] test/*.$(O) test/*.exe

TOINSTALL=\
  PRNG.mli PRNG.cmi PRNG.cmti \
  PRNG.cma PRNG.cmxa PRNG.$(A) libPRNG.$(A) dllPRNG.$(SO)

install:
	$(OCAMLFIND) install pringo META $(TOINSTALL)

uninstall:
	$(OCAMLFIND) remove pringo

testresults/us-%.log: test/u01.exe
	@mkdir -p testresults
	./test/u01.exe -small $* > $@

testresults/um-%.log: test/u01.exe
	@mkdir -p testresults
	./test/u01.exe -medium $* > $@

testresults/ub-%.log: test/u01.exe
	@mkdir -p testresults
	./test/u01.exe -big $* > $@

testresults/ur-%.log: test/u01.exe
	@mkdir -p testresults
	./test/u01.exe -rabbit $* > $@

testresults/ua-%.log: test/u01.exe
	@mkdir -p testresults
	./test/u01.exe -alphabit $* > $@

testresults/dh-%.log: test/generator.exe
	@mkdir -p testresults
	./test/generator.exe $* | $(DIEHARDER) > $@

testresults/ent-%.log: test/generator.exe
	@mkdir -p testresults
	./test/generator.exe $* | $(ENT) > $@

clean::
	rm -rf testresults

TESTS=float seq8 seq32 seq64 block-13 \
  treesplit-1 treesplit-4 laggedsplit-3 splita splits

ALLTESTS=$(TESTS:%=chacha-%) $(TESTS:%=splitmix-%) $(TESTS:%=lxm-%)

SMALLTESTS=$(ALLTESTS:%=testresults/us-%.log)

smalltest: $(SMALLTESTS)
	@test/reporting $(SMALLTESTS)

FULLTESTS=$(ALLTESTS:%=testresults/um-%.log) \
          $(ALLTESTS:%=testresults/ur-%.log) \
          $(ALLTESTS:%=testresults/ua-%.log)

fulltest: $(FULLTESTS)
	@test/reporting $(FULLTESTS)

HUGETESTS=$(ALLTESTS:%=testresults/ub-%.log)

hugetest: $(HUGETESTS)
	@test/reporting $(HUGETESTS)

consistencytest: test/consistency.exe
	./test/consistency.exe

benchmark: test/benchmark.exe
	./test/benchmark.exe

docs: *.mli
	mkdir -p docs
	ocamldoc -d docs/ -html *.mli

depend:
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend
