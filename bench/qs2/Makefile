BENCH = \
  arith \
  arithficial \
  arith-negate-abs \
  binarytree \
  bool \
  insertsort \
  list \
  list-ineq \
  plus-abs \
  plus-abs-ineq \
  regexes \
  zip

all: $(BENCH)

bench: all
	@echo -n "arith            "; /usr/bin/time -f%e0 ./arith            2>&1 >            arith.out
	@echo -n "arith-negate-abs "; /usr/bin/time -f%e0 ./arith-negate-abs 2>&1 > arith-negate-abs.out
	@echo -n "bool             "; /usr/bin/time -f%e0 ./bool             2>&1 >             bool.out
	@echo -n "list             "; /usr/bin/time -f%e0 ./list             2>&1 >             list.out
	@echo -n "insertsort       "; /usr/bin/time -f%e0 ./insertsort       2>&1 >       insertsort.out
	@echo -n "zip              "; /usr/bin/time -f%e0 ./zip              2>&1 >              zip.out
	@echo -n "arithficial      "; /usr/bin/time -f%e0 ./arithficial      2>&1 >      arithficial.out
	@echo -n "plus-abs         "; /usr/bin/time -f%e0 ./plus-abs         2>&1 >         plus-abs.out
	@echo -n "regexes          "; /usr/bin/time -f%e0 ./regexes          2>&1 >          regexes.out
	@echo -n "binarytree       "; /usr/bin/time -f%e0 ./binarytree       2>&1 >       binarytree.out

bench-binarytree: binarytree
	for i in {1..6}; do /usr/bin/time -f%e0 ./binarytree 2>&1 > binarytree.out$$i; done | tee binarytree.runtime-zero

save-bench: bench
	make -s bench > runtime-$$HOSTNAME

# this takes a few minutes
prepare:
	cabal sandbox init
	git clone https://github.com/nick8325/quickspec
	cabal sandbox add-source quickspec
	cabal install quickspec

prepare-algehed:
	cabal sandbox init
	git clone https://github.com/MaximilianAlgehed/quickspec
	# git -C quickspec checkout fd53aae3acdcba0e962a72bce652f102c13aaeb9
	cabal sandbox add-source quickspec
	cabal install quickspec

regexes: regexes.hs ../../eg/Regex.hs

%: %.hs
	ghc -O2 -i../../eg -package-db .cabal-sandbox/*-packages.conf.d $< && touch $@

clean:
	rm -rf *.o *.hi $(BENCH)

clean-prepare:
	rm -rf .cabal-sandbox cabal.sandbox.config quickspec
