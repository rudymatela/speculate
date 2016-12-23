Speculate
=========

Speculate automatically speculates (conditional-/semi-) equational properties
about given Haskell functions.
(it is similar to, and inspired by [QuickSpec].)

It is currently under development, expect it to break often for now.


Crash Course
------------

Install pre-requisites:

	$ cabal install cmdargs
	$ cabal install leancheck

Clone and enter the repository:

	$ git clone https://github.com/rudymatela/speculate
	$ cd speculate

There are some examples in the `eg` folter.  For example `eg/numbool`:

	$ cat eg/numbool
	...
	...

Compile it with:

	$ ghc -isrc eg/numbool.hs

Run it with:

	$ ./eg/numbool
	...

Pass `-C` to get conditional equations:

	$ ./eg/numbool -C
	...


Installing Speculate
--------------------

Pre-requisites are [cmdargs] and [leancheck].
You can install them with:

	$ cabal install cmdargs
	$ cabal install leancheck

No `cabal` package has been made yet.  For now, you clone the repository with:

	$ git clone https://github.com/rudymatela/speculate

and compile programs that use it with:

	$ ghc -ipath/to/speculate/src program.hs


Using Speculate
---------------

Speculate is used as a library: you import it, then call the function
`speculate` with relevant arguments.  The following program Speculates about
the functions `(+)` and `abs`:

	import Test.Speculate

	main :: IO ()
	main = speculate args
	  { atoms =
		  [ showConstant (0::Int)
		  , showConstant (1::Int)
		  , constant "+"   ((+)  :: Int -> Int -> Int)
		  , constant "abs" (abs  :: Int -> Int)
		  ]
	  }

when run (`./speculate`), it prints the following:

	_ :: Int  (holes: Int)
	0 :: Int
	1 :: Int
	(+) :: Int -> Int -> Int
	abs :: Int -> Int

	          abs 0 == 0
	          abs 1 == 1
	    abs (abs x) == abs x
	          x + 0 == x
	    abs (1 + 1) == 1 + 1
	    (x + y) + z == x + (y + z)
	abs (x + abs x) == x + abs x
	  abs x + abs x == abs (x + x)
	abs (1 + abs x) == 1 + abs x
	          x + y == y + x

	0 <= 1
	x <= abs x
	0 <= abs x
	x <= x + 1

Now, if we add the following to the list of atoms

	, constant "<="  ((<=) :: Int -> Int -> Bool)
	, constant "<"   ((<)  :: Int -> Int -> Bool)

then run again,
we get the following as well:

	    y <= x ==> abs (x + abs y) == x + abs y
	    x <= 0 ==>       x + abs x == 0
	    0 <= x ==>           abs x == x
	    0 <= x ==>     abs (x + 1) == x + 1
	    0 <= x ==> abs (x + abs y) == x + abs y
	abs x <= y ==>     abs (x + y) == x + y
	abs y <= x ==>     abs (x + y) == x + y

For more examples, see the [eg](eg) folder.


Similarities and Differences to QuickSpec
-----------------------------------------

Speculate is inspired by [QuickSpec].
Like QuickSpec, Speculate uses testing to speculate equational laws about given
Haskell functions.  There are some differences:

|                | Speculate      | QuickSpec                         |
| -------------: | -------------- | --------------------------------- |
| testing        | enumerative    | random                            |
| conditions     | "unrestricted" | restricted to a set of predicates |
| semi-equations | yes            | no                                |
| polymorphism   | no             | yes                               |
| performance    | slower         | faster                            |

For most examples, Speculate runs slower than QuickSpec 2 but faster than QuickSpec 1.


More documentation
------------------

For more examples, see the [eg](eg) and [bench](bench) folders.

[QuickSpec]: https://github.com/nick8325/quickspec
