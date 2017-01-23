# Makefile for Speculate
LEANCHECKPATH = ../leancheck
GHCIMPORTDIRS = src:eg:$(LEANCHECKPATH)/src:tests
GHCFLAGS = -O2 #-prof -auto-all #-caf-all
# When profiling is enabled, to get the cost centres with more than 6% time:
#   $ ./eg/arith  +RTS -p -RTS
#   $ cat arith.prof | grep -v ' [0-5].[0-9] ......$'
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
EG = \
  eg/arith \
  eg/0arith \
  eg/bool \
  eg/binarytree \
  eg/colour \
  eg/digraphs \
  eg/list \
  eg/length \
  eg/minus \
  eg/numbool \
  eg/insertsort \
  eg/string \
  eg/oddeven \
  eg/pretty \
  eg/ratio \
  eg/sets \
  eg/tauts \
  eg/monad \
  bench/arithficial
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

all: $(EG)

quick-test: $(patsubst %,%.test,$(QUICKTESTS)) \
            $(patsubst %,%.test-model,$(QUICKEG))

test: all $(patsubst %,%.test,$(TESTS)) \
          $(patsubst %,%.test-model,$(EG) $(wildcard bench/*-c))

legacy-test:
	make clean && make -C $(LEANCHECKPATH) clean && make -j8 GHC=ghc-7.10 && make quick-test -j8 GHC=ghc-7.10
	make clean && make -C $(LEANCHECKPATH) clean && make -j8 GHC=ghc-7.8  && make quick-test -j8 GHC=ghc-7.8
	make clean && make -C $(LEANCHECKPATH) clean && make -j8 GHC=ghc-7.6  && make quick-test -j8 GHC=ghc-7.6
	make clean && make -C $(LEANCHECKPATH) clean && make -j8 GHC=ghc-7.4  && make quick-test -j8 GHC=ghc-7.4
	make clean && make -C $(LEANCHECKPATH) clean && make -j8              && make slow-test  -j8

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

tests/Test.o: src/Test/Speculate.o

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: src/Test/Speculate.o tests/Test.o
	touch mk/toplibs

include mk/haskell.mk

%-3.dot: %
	./$< -dv3 > $@

%.dot: %
	./$< -dv2 > $@

%-2.dot: %
	./$< -dv2 > $@

%-1.dot: %
	./$< -dv1 > $@

%.eps: %.dot
	dot -Teps $< > $@

%.pdf: %.eps
	epstopdf $<

.PHONY: %.view
%.view: %.pdf
	o $<
