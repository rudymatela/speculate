# Makefile for Speculate
#
# Copyright:   (c) 2015-2019 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
GHCIMPORTDIRS = src:eg:test
GHCFLAGS = -O2 -v0 \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic -package cmdargs -package regex-tdfa -package algebraic-graphs)
# -Wall -Wno-name-shadowing -Wno-orphans -Wno-unused-matches
# -prof -auto-all #-caf-all
# When profiling is enabled, to get the cost centres with more than 6% time:
#   $ ./eg/arith  +RTS -p -RTS
#   $ cat arith.prof | grep -v ' [0-5].[0-9] ......$'
HADDOCKFLAGS = \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic --optghc=-package=cmdargs --optghc=-package=regex-tdfa --optghc=-package=algebraic-graphs)
MAXTESTS = 4000
MAXSIZE = -s4
TESTS = \
  test/creason \
  test/engine \
  test/eval \
  test/expr \
  test/order \
  test/reason \
  test/utils
EG = \
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
  eg/ratio \
  eg/sets \
  eg/tauts \
  eg/monad \
  eg/tuples \
  eg/speculate-reason \
  bench/arithficial \
  bench/lowtests \
  bench/nord \
  bench/trilean \
  bench/unit \
  bench/stats
EXTRAEG = \
  eg/pretty \
  eg/regexes \
  eg/algebraic-graphs
# regexes needs regex-tdfa, which may break the build
# speculate-reason output differs in different GHC versions
QUICKTESTS = \
  test/engine \
  test/eval \
  test/expr \
  test/order \
  test/reason
QUICKEG = \
  eg/arith \
  eg/bool \
  eg/list
BENCH = $(EG) $(wildcard bench/*-t) $(wildcard bench/*-c)
LIST_ALL_HSS = find src test eg bench/*.hs mk -name \*.hs | grep -vE 'eg/(regexes|pretty-compact|algebraic-graphs)'
LIST_LIB_HSS = find src -name \*.hs
LIB_DEPS = base template-haskell $(INSTALL_DEPS)
INSTALL_DEPS = leancheck express cmdargs containers

all: mk/toplibs

test: all test-sdist $(patsubst %,%.run,$(TESTS)) diff-test

test-with-extra-deps: test diff-test-extra

txt: $(patsubst %,%.txt,$(BENCH))

diff-test: $(patsubst %,%.diff,$(BENCH))

# Disclaimer: This bench target is not intended to generate paper-grade runtime
#             datapoints as it runs each benchmark just once.  This target is
#             meant to track large runtime changes across different git
#             versions.
.PHONY: bench
bench: $(EG) $(patsubst %,%.bench,$(BENCH))
	@mkdir -p bench/runtime/$$HOSTNAME
	./bench/versions $(INSTALL_DEPS) | tee bench/runtime/$$HOSTNAME/versions

txt-extra: $(EXTRAEG) $(patsubst %,%.txt,$(EXTRAEG))

diff-test-extra: $(EXTRAEG) $(patsubst %,%.diff,$(EXTRAEG))

test-sdist:
	./test/sdist

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test engine

test-via-stack:
	stack test speculate:test:engine --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

legacy-test:
	make clean  &&  make -j8 GHC=ghc-8.2   &&  make quick-test -j8 GHC=ghc-8.2
	make clean  &&  make -j8 GHC=ghc-8.0   &&  make quick-test -j8 GHC=ghc-8.0
	make clean  &&  make -j8 GHC=ghc-7.10  &&  make quick-test -j8 GHC=ghc-7.10
	make clean  &&  make -j8 GHC=ghc-7.8   &&  make quick-test -j8 GHC=ghc-7.8
	make clean  &&  make -j8               &&  make slow-test  -j8

legacy-test-via-cabal:
	cabal clean  &&  cabal-ghc-8.2  configure  &&  cabal-ghc-8.2  test
	cabal clean  &&  cabal-ghc-8.0  configure  &&  cabal-ghc-8.0  test
	cabal clean  &&  cabal-ghc-7.10 configure  &&  cabal-ghc-7.10 test
	cabal clean  &&  cabal-ghc-7.8  configure  &&  cabal-ghc-7.8  test
	cabal clean  &&  cabal test

prepare:
	cabal v1-install cmdargs leancheck express regex-tdfa algebraic-graphs pretty-compact

prepare-legacy-test: \
  prepare-legacy-test-8.2 \
  prepare-legacy-test-8.0 \
  prepare-legacy-test-7.10 \
  prepare-legacy-test-7.8

prepare-legacy-test-8.2:
	cabal-ghc-8.2 update
	cabal-ghc-8.2 --ignore-sandbox install cmdargs leancheck

prepare-legacy-test-8.0:
	cabal-ghc-8.0 update
	cabal-ghc-8.0 --ignore-sandbox install cmdargs leancheck

prepare-legacy-test-7.10:
	cabal-ghc-7.10 update
	cabal-ghc-7.10 --ignore-sandbox install cmdargs-0.10.17 leancheck

prepare-legacy-test-7.8:
	cabal-ghc-7.8 update
	cabal-ghc-7.8  --ignore-sandbox install cmdargs-0.10.17 leancheck

slow-test: MAXTESTS =
slow-test: MAXSIZE =
slow-test: test

%.run: %
	./$< $(MAXTESTS)

%.txt: %
	./$< >$<.txt

%.diff: %
	./$< | diff -rud $<.txt -

bench/%-c: eg/%
	touch $@

bench/%-t: eg/%
	touch $@

%.bench: %
	@mkdir -p bench/runtime/$$HOSTNAME/`dirname $<`
	@printf "%-20s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | \
	tee bench/runtime/$$HOSTNAME/$<.runtime

update-listable-expr:
	cp -rav ../express/test/Test/ListableExpr.hs test/Test/

ghci: test/Test.ghci

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(EG) eg/*.dot eg/*.pdf TAGS tags mk/toplibs mk/Toplibs.{o,hi}
	rm -f doc/*.html doc/*.gif doc/*.css doc/*.js doc/*.png

test/Test.o: src/Test/Speculate.o

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and test/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
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
