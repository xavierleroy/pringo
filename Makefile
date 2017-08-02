OCAMLFLAGS=-g -safe-string
OCAMLC=ocamlc $(OCAMLFLAGS)
OCAMLOPT=ocamlopt $(OCAMLFLAGS)
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib

DIEHARDER=dieharder -g 200 -a
ENT=head -c 1000000 | ent

all: PRNG.cmxa PRNG.cma

PRNG.cmxa PRNG.cma: PRNG.cmx PRNG.cmo stubs.o
	$(OCAMLMKLIB) -o PRNG PRNG.cmo PRNG.cmx stubs.o

%.cmx: %.ml
	$(OCAMLOPT) -c $*.ml
%.cmo: %.ml
	$(OCAMLC) -c $*.ml
%.cmi: %.mli
	$(OCAMLOPT) -c $*.mli
%.o: %.c
	$(OCAMLC) -c $*.c
%.exe: %.ml PRNG.cmxa
	$(OCAMLOPT) -I . -o $@ PRNG.cmxa $*.ml

clean::
	rm -f *.cm[ioxa] *.cmxa *.o *.a
	rm -f test/*.cm[iox] test/*.o test/*.exe

testresults/dh-%.log: test/generator.exe testresults
	./test/generator.exe -$(subst -, ,$*) | $(DIEHARDER) > $@

testresults/ent-%.log: test/generator.exe testresults
	./test/generator.exe -$(subst -, ,$*) | $(ENT) > $@

clean::
	rm -rf testresults

testresults:
	mkdir testresults

ALLTESTS=seq8 seq32 seq64 block-13 \
  treesplit-1 treesplit-2 treesplit-4 \
  laggedsplit-1 laggedsplit-3 laggedsplit-10

SMALLTESTS=$(ALLTESTS:%=testresults/ent-%.log)

smalltest: $(SMALLTESTS)
	@grep 'would exceed' $(SMALLTESTS) | sed -e 's/would exceed this value//'

FULLTESTS=$(ALLTESTS:%=testresults/dh-%.log)
fulltest: $(FULLTESTS)
	@printf "PASSED: "; cat $(FULLTESTS) | grep -c PASSED
	@printf "WEAK: "; cat $(FULLTESTS) | grep -c WEAK
	@if grep FAIL $(FULLTESTS); then exit 2; else exit 0; fi

consistencytest: test/consistency.exe
	./test/consistency.exe

benchmark: test/benchmark.exe
	./test/benchmark.exe

depend:
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend
