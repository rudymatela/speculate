BENCH = \
  0arith \
  arith \
  bool \
  list \
  arithficial

all: $(BENCH)

bench: all
	@echo -n "0arith       "; /usr/bin/time -f%e0 ./0arith      2>&1 >      0arith.out
	@echo -n "arith        "; /usr/bin/time -f%e0 ./arith       2>&1 >       arith.out
	@echo -n "bool         "; /usr/bin/time -f%e0 ./bool        2>&1 >        bool.out
	@echo -n "list         "; /usr/bin/time -f%e0 ./list        2>&1 >        list.out
	@echo -n "arithficial  "; /usr/bin/time -f%e0 ./arithficial 2>&1 > arithficial.out

save-bench: bench
	make bench > runtime-$$HOSTNAME

# this takes a few minutes
prepare:
	cabal sandbox init
	git clone https://github.com/nick8325/quickspec
	cabal sandbox add-source quickspec
	cabal install quickspec

%: %.hs
	ghc -O2 -package-db .cabal-sandbox/*-packages.conf.d $< && touch $@

clean:
	rm -rf *.o *.hi $(BENCH)

clean-prepare:
	rm -rf .cabal-sandbox cabal.sandbox.config quickspec
