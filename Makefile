# Makefile for Speculate
#
# Copyright:   (c) 2015-2018 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
GHCIMPORTDIRS = src:eg:tests
GHCFLAGS = -O2 -Wall -Wno-name-shadowing -Wno-orphans -Wno-unused-matches \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
# -prof -auto-all #-caf-all
# When profiling is enabled, to get the cost centres with more than 6% time:
#   $ ./eg/arith  +RTS -p -RTS
#   $ cat arith.prof | grep -v ' [0-5].[0-9] ......$'
HADDOCKFLAGS = --no-print-missing-docs \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic)
MAXTESTS = 4000
MAXSIZE = -s4
TESTS = \
  tests/test-creason \
  tests/test-engine \
  tests/test-eval \
  tests/test-expr \
  tests/test-match \
  tests/test-order \
  tests/test-reason \
  tests/test-utils \
  tests/test-stats
MOSTEG = \
  eg/arith \
  eg/arith-negate-abs \
  eg/bool \
  eg/binarytree \
  eg/binarytree0 \
  eg/colour \
  eg/digraphs \
  eg/fun \
  eg/list \
  eg/length \
  eg/zip \
  eg/minus \
  eg/insertsort \
  eg/insertsort0 \
  eg/string \
  eg/oddeven \
  eg/plus-abs \
  eg/pretty \
  eg/ratio \
  eg/sets \
  eg/tauts \
  eg/monad \
  eg/tuples \
  bench/arithficial \
  bench/nord \
  bench/unit
EG = $(MOSTEG) \
  eg/regexes \
  eg/speculate-reason
EXTRAEG = $(EG) \
  eg/pretty-compact \
  eg/algebraic-graphs
# regexes needs regex-tdfa, which may break the build
# speculate-reason output differs in different GHC versions
QUICKTESTS = \
  tests/test-engine \
  tests/test-eval \
  tests/test-expr \
  tests/test-match \
  tests/test-order \
  tests/test-reason
QUICKEG = \
  eg/arith \
  eg/bool \
  eg/list
LIST_ALL_HSS = find src tests eg bench/*.hs -name \*.hs
LIST_LIB_HSS = find src -name \*.hs

all: mk/toplibs

quick-test: $(patsubst %,%.test,$(QUICKTESTS)) \
            $(patsubst %,%.test-model,$(QUICKEG))

test: all $(patsubst %,%.test,$(TESTS)) \
          $(patsubst %,%.test-model,$(EG) $(wildcard bench/*-c))

test-without-extra-deps: all $(patsubst %,%.test,$(TESTS)) \
                             $(patsubst %,%.test-model,$(MOSTEG) $(wildcard bench/*-c))

test-extra-deps: all $(patsubst %,%.test,$(TESTS)) \
                     $(patsubst %,%.test-model,$(EXTRAEG) $(wildcard bench/*-c))

test-sdist:
	./tests/test-sdist

legacy-test:
	make clean  &&  make -j8 GHC=ghc-8.2   &&  make quick-test -j8 GHC=ghc-8.2
	make clean  &&  make -j8 GHC=ghc-8.0   &&  make quick-test -j8 GHC=ghc-8.0
	make clean  &&  make -j8 GHC=ghc-7.10  &&  make quick-test -j8 GHC=ghc-7.10
	make clean  &&  make -j8 GHC=ghc-7.8   &&  make quick-test -j8 GHC=ghc-7.8
	make clean  &&  make -j8               &&  make slow-test  -j8

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="-dynamic -Werror" && cabal build && cabal test

legacy-test-via-cabal:
	cabal clean  &&  cabal-ghc-8.2  configure  &&  cabal-ghc-8.2  test
	cabal clean  &&  cabal-ghc-8.0  configure  &&  cabal-ghc-8.0  test
	cabal clean  &&  cabal-ghc-7.10 configure  &&  cabal-ghc-7.10 test
	cabal clean  &&  cabal-ghc-7.8  configure  &&  cabal-ghc-7.8  test
	cabal clean  &&  cabal test

prepare-test:
	cabal --ignore-sandbox install regex-tdfa cmdargs leancheck algebraic-graphs pretty-compact

prepare-legacy-test: \
  prepare-legacy-test-8.2 \
  prepare-legacy-test-8.0 \
  prepare-legacy-test-7.10 \
  prepare-legacy-test-7.8

prepare-legacy-test-8.2:
	cabal-ghc-8.2 update
	cabal-ghc-8.2 --ignore-sandbox install regex-tdfa cmdargs leancheck

prepare-legacy-test-8.0:
	cabal-ghc-8.0 update
	cabal-ghc-8.0 --ignore-sandbox install regex-tdfa cmdargs leancheck

prepare-legacy-test-7.10:
	cabal-ghc-7.10 update
	cabal-ghc-7.10 --ignore-sandbox install regex-tdfa cmdargs leancheck

prepare-legacy-test-7.8:
	cabal-ghc-7.8 update
	cabal-ghc-7.8  --ignore-sandbox install regex-tdfa cmdargs leancheck

slow-test: MAXTESTS =
slow-test: MAXSIZE =
slow-test: test

%.test: %
	./$< $(MAXTESTS)

bench/%-c.test-model: eg/%
	./tests/test-model $(MAXSIZE) bench/$*-c

bench/%-c.update-4-test-model: %
	./tests/update-test-model -s4 bench/$*-c

bench/%-c.update-slow-test-model: %
	./tests/update-test-model     bench/$*-c

%.test-model: %
	./tests/test-model $(MAXSIZE) $<

%.update-test-model: %
	./tests/update-test-model -s4 $<
	./tests/update-test-model     $<

%.update-4-test-model: %
	./tests/update-test-model -s4 $<

%.update-slow-test-model: %
	./tests/update-test-model     $<

%.update-7-test-model: %
	./tests/update-test-model -s7 $<

update-test-model: update-4-test-model update-slow-test-model

update-4-test-model: $(patsubst %,%.update-4-test-model,$(EG) $(wildcard bench/*-c))

update-slow-test-model: $(patsubst %,%.update-slow-test-model,$(EG) $(wildcard bench/*-c))

bench: all
	./tests/benchmark-cmp $(EG) bench/*-c

save-bench: all
	./tests/benchmark-save $(EG) bench/*-c

memory-benchmark: all
	./tests/memory-benchmark $(EG) bench/*-c

qs-bench:
	make -sC bench/qs1 bench
	make -sC bench/qs2 bench

qs-save-bench:
	make -sC bench/qs1 save-bench
	make -sC bench/qs2 save-bench

update-listable-expr:
	cp -rav ../haexpress/test/Test/ListableExpr.hs tests/Test/

ghci: tests/Test.ghci

clean: clean-hi-o
	rm -f $(TESTS) $(EG) eg/*.dot eg/*.pdf TAGS tags mk/toplibs
	make clean -C bench/qs1
	make clean -C bench/qs2
	rm -f doc/*.html doc/*.gif doc/*.css doc/*.js doc/*.png

tests/Test.o: src/Test/Speculate.o

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: src/Test/Speculate.o tests/Test.o
	touch mk/toplibs

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Use first" \
	  --ignore "Use second" \
	  --ignore "Use ***" \
	  src bench tests

include mk/haskell.mk

%-3.dot: %
	./$< -gv3 > $@

%.dot: %
	./$< -gv2 > $@

%-2.dot: %
	./$< -gv2 > $@

%-1.dot: %
	./$< -gv1 > $@

%.eps: %.dot
	dot -Teps $< > $@

%.pdf: %.eps
	epstopdf $<

.PHONY: %.view
%.view: %.pdf
	o $<
