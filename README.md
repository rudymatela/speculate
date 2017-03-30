Speculate
=========

[![Speculate Build Status][build-status]][build-log]
[![Speculate on Hackage][hackage-version]][speculate-on-hackage]

Speculate automatically discovers laws about Haskell functions.
Give Speculate a bunch of Haskell functions and it will discover laws like:

  * equations, such as `id x == x`;
  * inequalities, such as `0 <= x * x`;
  * conditional equations, such as `x <= 0  ==>  x + abs x == 0`.

Speculate is similar to, and inspired by, [QuickSpec].


Crash Course
------------

Install pre-requisites:

	$ cabal install cmdargs
	$ cabal install leancheck

Clone and enter the repository:

	$ git clone https://github.com/rudymatela/speculate
	$ cd speculate

There are some examples in the `eg` folter.  For example `eg/plus-abs.hs`:

	$ cat eg/plus-abs.hs
	...
	...

Compile and run with:

	$ ghc -isrc eg/plus-abs.hs
	$ ./eg/plus-abs
	...


Installing Speculate
--------------------

Pre-requisites are [cmdargs] and [leancheck].
You can install them with:

	$ cabal install cmdargs
	$ cabal install leancheck

No `cabal` package has been made yet.  For now, clone the repository with:

	$ git clone https://github.com/rudymatela/speculate

and compile programs that use it with:

	$ ghc -ipath/to/speculate/src program.hs


Using Speculate
---------------

Speculate is used as a library: import it, then call the function `speculate`
with relevant arguments.  The following program Speculates about the functions
`(+)` and `abs`:

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


Now, if we add `<=` and `<` as background constants on `args`

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

|                   | Speculate                 | QuickSpec                         |
| ----------------: | ------------------------- | --------------------------------- |
| testing           | enumerative ([LeanCheck]) | random ([QuickCheck])             |
| equational laws   | yes (after completion)    | yes (as discovered)               |
| inequational laws | yes                       | no                                |
| conditional laws  | yes                       | restricted to a set of predicates |
| polymorphism      | no                        | yes                               |
| performance       | slower                    | faster                            |

For most examples, Speculate runs slower than QuickSpec 2 but faster than QuickSpec 1.


More documentation
------------------

For more examples, see the [eg](eg) and [bench](bench) folders.

[leancheck]: https://hackage.haskell.org/package/leancheck
[LeanCheck]: https://hackage.haskell.org/package/leancheck
[QuickSpec]: https://github.com/nick8325/quickspec
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[cmdargs]: https://hackage.haskell.org/package/cmdargs

[build-status]: https://travis-ci.org/rudymatela/speculate.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/speculate
[hackage-version]: https://img.shields.io/hackage/v/speculate.svg
[speculate-on-hackage]: https://hackage.haskell.org/package/speculate
