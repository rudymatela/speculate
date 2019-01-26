Speculate
=========

[![Speculate Build Status][build-status]][build-log]
[![Speculate on Hackage][hackage-version]][speculate-on-hackage]
[![Speculate on Stackage LTS][stackage-lts-badge]][speculate-on-stackage-lts]
[![Speculate on Stackage Nightly][stackage-nightly-badge]][speculate-on-stackage-nightly]

![Speculate logo][speculate-logo]

Speculate automatically discovers laws about [Haskell] functions.
Give Speculate a bunch of Haskell functions and it will discover laws like:

  * equations, such as `id x == x`;
  * inequalities, such as `0 <= x * x`;
  * conditional equations, such as `x <= 0  ==>  x + abs x == 0`.

Speculate is similar to, and inspired by, [QuickSpec].


Installing Speculate
--------------------

To install the [latest Speculate version from Hackage], just:

	$ cabal update
	$ cabal install speculate

Pre-requisites are [cmdargs] and [leancheck].
They should be automatically resolved and installed by [Cabal].


Using Speculate
---------------

Speculate is used as a library: import it, then call the function [`speculate`]
with relevant arguments.  The following program Speculates about the
functions [`(+)`] and [`abs`]:

	import Test.Speculate

	main :: IO ()
	main = speculate args
	  { constants =
	      [ showConstant (0::Int)
	      , showConstant (1::Int)
	      , constant "+"   ((+)  :: Int -> Int -> Int)
	      , constant "abs" (abs  :: Int -> Int)
	      ]
	  }

when run, it prints the following:

	_ :: Int  (holes: Int)
	0 :: Int
	1 :: Int
	(+) :: Int -> Int -> Int
	abs :: Int -> Int

	    abs (abs x) == abs x
	          x + 0 == x
	          x + y == y + x
	    (x + y) + z == x + (y + z)
	abs (x + abs x) == x + abs x
	  abs x + abs x == abs (x + x)
	abs (1 + abs x) == 1 + abs x

	x <= abs x
	0 <= abs x
	x <= x + 1


Now, if we add [`<=`] and [`<`] as background constants on [`args`]

	  , constants =
	      [ showConstant (0::Int)
	      , showConstant (1::Int)
	      , constant "+"   ((+)  :: Int -> Int -> Int)
	      , constant "abs" (abs  :: Int -> Int)
	      , background
	      , constant "<="  ((<=) :: Int -> Int -> Bool)
	      , constant "<"   ((<)  :: Int -> Int -> Bool)
	      ]

then run again, we get the following as well:

	    y <= x ==> abs (x + abs y) == x + abs y
	    x <= 0 ==>       x + abs x == 0
	abs x <= y ==>     abs (x + y) == x + y
	abs y <= x ==>     abs (x + y) == x + y

For more examples, see the [eg](eg) folder.


Similarities and Differences to QuickSpec
-----------------------------------------

Speculate is inspired by [QuickSpec].
Like QuickSpec, Speculate uses testing to speculate equational laws about given
Haskell functions.  There are some differences:

* Speculate tests enumeratively using [LeanCheck],
  QuickSpec tests randomly using [QuickCheck];
* Speculate is able to report inequalities directly;
* QuickSpec allows polymorphism, Speculate does not;
* For most examples,
  Speculate runs slower than QuickSpec 2
  but faster than QuickSpec 1.


More documentation
------------------

For more examples, see the [eg](eg) and [bench](bench) folders.

Speculate has been subject to a paper, see the
[Speculate Paper on Haskell Symposium 2017](https://matela.com.br/paper/speculate.pdf).
Speculate is also subject to a chapter in a [PhD Thesis (2017)].

[leancheck]: https://hackage.haskell.org/package/leancheck
[LeanCheck]: https://hackage.haskell.org/package/leancheck
[QuickSpec]: https://github.com/nick8325/quickspec
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[cmdargs]: https://hackage.haskell.org/package/cmdargs

[Cabal]:   https://www.haskell.org/cabal
[Haskell]: https://www.haskell.org/

[PhD Thesis (2017)]: https://matela.com.br/paper/rudy-phd-thesis-2017.pdf

[`speculate`]: https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:speculate
[`args`]:      https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:args
[`constant`]:  https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:constant

[`(+)`]:       https://hackage.haskell.org/package/base/docs/Prelude.html#v:-43-
[`abs`]:       https://hackage.haskell.org/package/base/docs/Prelude.html#v:abs
[`<=`]:        https://hackage.haskell.org/package/base/docs/Prelude.html#v:-60--61-
[`<`]:         https://hackage.haskell.org/package/base/docs/Prelude.html#v:-60-

[speculate-logo]: https://github.com/rudymatela/speculate/raw/master/doc/speculate.svg?sanitize=true

[build-status]: https://travis-ci.org/rudymatela/speculate.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/speculate
[hackage-version]: https://img.shields.io/hackage/v/speculate.svg
[latest Speculate version from Hackage]: https://hackage.haskell.org/package/speculate
[speculate-on-hackage]:                  https://hackage.haskell.org/package/speculate
[stackage-lts-badge]:            https://stackage.org/package/speculate/badge/lts
[stackage-nightly-badge]:        https://stackage.org/package/speculate/badge/nightly
[speculate-on-stackage]:         https://stackage.org/package/speculate
[speculate-on-stackage-lts]:     https://stackage.org/lts/package/speculate
[speculate-on-stackage-nightly]: https://stackage.org/nightly/package/speculate
