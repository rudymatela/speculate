# Makefile for Speculate
GHCIMPORTDIRS = src:eg:tests
GHCFLAGS = -O2 -dynamic #-prof -auto-all #-caf-all
# When profiling is enabled, to get the cost centres with more than 6% time:
#   $ ./eg/arith  +RTS -p -RTS
#   $ cat arith.prof | grep -v ' [0-5].[0-9] ......$'
HADDOCKFLAGS = --no-print-missing-docs
MAXTESTS = 4000
MAXSIZE = -s4
TESTS = \
  tests/test-creason \
  tests/test-engine \
  tests/test-eval \
  tests/test-expr \
  tests/test-match \
  tests/test-misc \
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
  bench/arithficial
EG = $(MOSTEG) \
  eg/regexes \
  eg/speculate-reason
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
LISTHS = find src tests eg bench/*.hs -name \*.hs
HSS = $(shell $(LISTHS))
LISTLIBS=find src -name \*.hs

all: $(EG)

quick-test: $(patsubst %,%.test,$(QUICKTESTS)) \
            $(patsubst %,%.test-model,$(QUICKEG))

test: all $(patsubst %,%.test,$(TESTS)) \
          $(patsubst %,%.test-model,$(EG) $(wildcard bench/*-c))

test-without-extra-deps: all $(patsubst %,%.test,$(TESTS)) \
                             $(patsubst %,%.test-model,$(MOSTEG) $(wildcard bench/*-c))

legacy-test:
	make clean && make -j8 GHC=ghc-7.10 && make quick-test -j8 GHC=ghc-7.10
	make clean && make -j8 GHC=ghc-7.8  && make quick-test -j8 GHC=ghc-7.8
	make clean && make -j8              && make slow-test  -j8

prepare-legacy-test: prepare-legacy-test-7.10 prepare-legacy-test-7.8 prepare-legacy-test-7.6 prepare-legacy-test-7.4

prepare-legacy-test-7.10:
	cabal-ghc-7.10 --ignore-sandbox install regex-tdfa cmdargs leancheck

prepare-legacy-test-7.8:
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

%.update-4-test-model: %
	./tests/update-test-model -s4 $<

%.update-slow-test-model: %
	./tests/update-test-model     $<

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

ghci: tests/Test.ghci

list-hs:
	$(LISTHS)

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

haddock: doc/index.html

clean-haddock:
	rm -f doc/*.{html,css,js,png,gif}

upload-haddock:
	@echo "use \`cabal upload -d' instead"
	@echo "(but 1st: cabal install --only-dependencies --enable-documentation)"
	@echo "(to just compile docs: cabal haddock --for-hackage)"

doc/index.html: $(shell $(LISTLIBS))
	./mk/haddock-i base template-haskell | xargs \
	haddock --html $(HADDOCKFLAGS) --title=speculate \
	  --optghc=-i$(GHCIMPORTDIRS) \
	  -odoc $(shell $(LISTLIBS))

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
