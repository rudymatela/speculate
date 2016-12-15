Speculate
=========

Speculate automatically speculates (conditional-/semi-) equational properties
about given Haskell functions.

(i.e.: [QuickSpec] with conditional- and semi- equations but without
polymorphism.).

It is currently under development, expect it to break often for now.


Installing Speculate
--------------------

Pre-requisites are [cmdargs] and [leancheck].
You can install them with:

	$ cabal install cmdargs
	$ cabal install leancheck

No `cabal` package has been made yet.  For now, you can compile programs that
use it with `ghc -Ipath/to/speculate/src`.


Using Speculate
---------------

Speculate is used as a library: you import it, then call the function
`speculate` with relevant arguments.  See the `eg` folder for examples.

Watch this space for a quick-tutorial in the near future.


More documentation
------------------

For more examples, see the [eg](eg) and [bench](bench) folders.
