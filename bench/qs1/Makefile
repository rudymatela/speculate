BENCH = \
  arith \
  arith-negate-abs \
  bool \
  list \
  regexes \
  arithficial

all: $(BENCH)

bench: all
	@echo -n "arith            "; /usr/bin/time -f%e0 ./arith            2>&1 >            arith.out
	@echo -n "arith-negate-abs "; /usr/bin/time -f%e0 ./arith-negate-abs 2>&1 > arith-negate-abs.out
	@echo -n "bool             "; /usr/bin/time -f%e0 ./bool             2>&1 >             bool.out
	@echo -n "list             "; /usr/bin/time -f%e0 ./list             2>&1 >             list.out
	@echo -n "arithficial      "; /usr/bin/time -f%e0 ./arithficial      2>&1 >      arithficial.out
	@echo "This will take a couple minutes to run (regexes)" > /dev/stderr
	@echo -n "regexes          "; /usr/bin/time -f%e0 ./regexes          2>&1 >          regexes.out

save-bench:
	make -s bench > runtime-$$HOSTNAME


prepare:
	cabal sandbox init
	cabal install quickspec-0.9.6



regexes: regexes.hs ../../eg/Regex.hs

%: %.hs
	ghc -O2 -i../../eg -package-db .cabal-sandbox/*-packages.conf.d $< && touch $@

clean:
	rm -rf *.o *.hi $(BENCH)

clean-prepare:
	rm -rf .cabal-sandbox cabal.sandbox.config
