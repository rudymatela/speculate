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
  * relations of order, such as `0 <= x * x`;
  * conditional equations, such as `x <= 0  ==>  x + abs x == 0`.

Speculate is similar to, and inspired by, [QuickSpec].


Installing Speculate
--------------------

To install the [latest Speculate version from Hackage], just:

	$ cabal update
	$ cabal install speculate

Pre-requisites are [cmdargs], [express] and [leancheck].
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

(One can use the [TypeApplications] to simplify the above examples:
`((+) @ Int)` instead of `((+) :: Int -> Int -> Int))`.
I have chosen to keep the example [Haskell 98] compliant.)


Supported types
---------------

Speculate works for virtually any type.
However,
if you would like to produce equations,
comparisons and variables of any given type
this type must be respectively
an instance of the [`Eq`], [`Ord`], [`Listable`] and [`Name`] typeclasses.

By default,
Speculate will produce equations, comparison and variables
to [a few types](https://github.com/rudymatela/speculate/blob/master/src/Test/Speculate/Expr/Instance.hs#L110-L151)
in the [Haskell 2010 Language Report].
If you would like expand that to more types,
you need to pass reified instances to Speculate explicitly by
using [`reifyInstances`] on [`instances =`] of [`speculate`]'s [`args`] like so:

	main = speculate args
      { instances = [ reifyInstances (undefined :: <Type1>)
                    , reifyInstances (undefined :: <Type2>)
                    , reifyInstances (undefined :: <Type3>)
	                , ...
	                ]
      , constants = ...
	  , ...
	  }

To use [`reifyInstances`],
your type must be an instance of
[`Eq`], [`Ord`], [`Listable`] and [`Name`].

* [`Eq`]  is needed for equations between values of the type;

* [`Ord`] is needed for comparisons between values of the type;

* [`Listable`] is needed for involving variables of the type.
  This is needed in order for Speculate to be able
  to generate values of your type to replace any variables.
  [LeanCheck] comes with [`Listable`] instances
  for virtually all types in the [Haskell 2010 Language Report].

* [`Name`] is needed for cosmetic puposes:
  if there are any variables of your type,
  [`Name`] allows you to tell Speculate how to call your variables.
  For example, if you have an `User` type, you can define your name instance as:

		instance Name (User) where
			name u  =  "usr"

  This way, variables of your `User` type will be called:
  `usr`, `usr1`, `usr2`, `usr3`, etc.

It is also fine to have only one, two or three of the above instances.
In that case, instead of [`reifyInstances`]
you can use [`reifyEq`], [`reifyOrd`], [`reifyListable`] and [`reifyName`] accordingly.
If you do not provide a [`Name`] implementation,
your variables will default to being `x`, `y` and `z`.
This may cause confusion as you involve more and more types,
compare the following two identical equations:

	[x,y] `areOwnedBy` z  ==  z `owns` x && z `owns` y
	[tckt,tckt1] `areOwnedBy` user  ==  usr `owns` tckt && user `owns tckt1`

The second is clearer.
So, I recomment you add a [`Name`] instance.
It is simple enough.

You also have to do this for any user defined types you are using
or even for newtypes.

Speculate comes with a few examples illustrating the use of [`reifyInstances`]:
on the [eg](eg) folder:
[eg/algebraic-graphs.hs](eg/algebraic-graphs.hs),
[eg/binarytree0.hs](eg/binarytree0.hs),
[eg/binarytree.hs](eg/binarytree.hs),
[eg/colour.hs](eg/colour.hs),
[eg/digraphs.hs](eg/digraphs.hs),
[eg/fun.hs](eg/fun.hs),
[eg/monad.hs](eg/monad.hs),
[eg/pretty-compact.hs](eg/pretty-compact.hs),
[eg/pretty.hs](eg/pretty.hs),
[eg/regexes.hs](eg/regexes.hs),
[eg/sets.hs](eg/sets.hs),
[eg/speculate-reason.hs](eg/speculate-reason.hs),
[eg/string.hs](eg/string.hs),
[eg/tauts.hs](eg/tauts.hs),
[eg/tuples.hs](eg/tuples.hs),
[eg/zip.hs](eg/zip.hs).

Not having the reified instances for a given type will cause the following warnings to be printed:

	Warning: no Listable instance for <YourTypeHere>, variables of this type will not be considered
	Warning: no Listable instance for <YourTypeHere>, variables of this type will not be considered
	Warning: no Eq instance for <YourTypeHere>, equations of this type will not be considered
	Warning: no Eq instance for <YourTypeHere>, equations of this type will not be considered
	Warning: no Ord instance for <YourTypeHere>, inequations of this type will not be considered
	Warning: no Ord instance for <YourTypeHere>, inequations of this type will not be considered

You can silence the above warnings by following the instructions above.
However, it may be the case that you don't want variables, equations or comparisons for a given type.
If that is so, you can ignore these warnings.


Similarities and Differences to QuickSpec
-----------------------------------------

Speculate is inspired by [QuickSpec].
Like QuickSpec, Speculate uses testing to speculate equational laws about given
Haskell functions.  There are some differences:

* Speculate tests enumeratively using [LeanCheck],
  QuickSpec tests randomly using [QuickCheck];
* Speculate is able to report comparisons directly;
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
[express]:   https://hackage.haskell.org/package/express
[QuickSpec]: https://github.com/nick8325/quickspec
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[cmdargs]: https://hackage.haskell.org/package/cmdargs

[Cabal]:   https://www.haskell.org/cabal
[Haskell]: https://www.haskell.org/

[PhD Thesis (2017)]: https://matela.com.br/paper/rudy-phd-thesis-2017.pdf

[`speculate`]:      https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:speculate
[`constant`]:       https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:constant
[`args`]:           https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:args
[`Args`]:           https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#t:Args
[`instances =`]:    https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#t:Args

[`reifyInstances`]: https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:reifyInstances
[`reifyEq`]:        https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:reifyEq
[`reifyOrd`]:       https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:reifyOrd
[`reifyEqOrd`]:     https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:reifyEqOrd
[`reifyListable`]:  https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:reifyListable
[`reifyName`]:      https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#v:reifyName

[`Eq`]:             https://hackage.haskell.org/package/base/docs/Prelude.html#t:Eq
[`Ord`]:            https://hackage.haskell.org/package/base/docs/Prelude.html#t:Ord
[`Listable`]:       https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Listable
[`Name`]:           https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html#t:Name

[`(+)`]:       https://hackage.haskell.org/package/base/docs/Prelude.html#v:-43-
[`abs`]:       https://hackage.haskell.org/package/base/docs/Prelude.html#v:abs
[`<=`]:        https://hackage.haskell.org/package/base/docs/Prelude.html#v:-60--61-
[`<`]:         https://hackage.haskell.org/package/base/docs/Prelude.html#v:-60-

[Haskell 2010 Language Report]:          https://www.haskell.org/onlinereport/haskell2010/
[Haskell 2010]:                          https://www.haskell.org/onlinereport/haskell2010/
[Haskell 98]:                            https://www.haskell.org/onlinereport/
[TypeApplications]:                      https://gitlab.haskell.org/ghc/ghc/-/wikis/type-application

[speculate-logo]: https://github.com/rudymatela/speculate/raw/master/doc/speculate.svg?sanitize=true

[build-log]:    https://github.com/rudymatela/speculate/actions/workflows/build.yml
[build-status]: https://github.com/rudymatela/speculate/actions/workflows/build.yml/badge.svg
[hackage-version]: https://img.shields.io/hackage/v/speculate.svg
[latest Speculate version from Hackage]: https://hackage.haskell.org/package/speculate
[speculate-on-hackage]:                  https://hackage.haskell.org/package/speculate
[stackage-lts-badge]:            https://stackage.org/package/speculate/badge/lts
[stackage-nightly-badge]:        https://stackage.org/package/speculate/badge/nightly
[speculate-on-stackage]:         https://stackage.org/package/speculate
[speculate-on-stackage-lts]:     https://stackage.org/lts/package/speculate
[speculate-on-stackage-nightly]: https://stackage.org/nightly/package/speculate
