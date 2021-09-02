Changelog for Speculate
=======================


v0.4.14
-------

* `Test.Speculate.Reason`: add `doubleCheck`;
* add `lowtests` benchmark;
* bump [express] requirement to v1.0.0;
* fix parallel compilation when using the Makefile.


v0.4.12
-------

* bump [express] requirement to v0.2.0
* add this changelog


v0.4.10
-------

* no changes in the actual [Speculate] library
* cleanup build files
* remove uneeded typeable derivations on examples and tests


v0.4.8
------

* no changes in the actual [Speculate] library
* refactor build scripts
* use GitHub Workflows as the CI
* fix compilation of some examples under the new [LeanCheck]


v0.4.6
------

* [`Test.Speculate`]: export `reifyName`;
* "internal" modules:
	- `Test.Speculate.Args`: remove `compareExpr`;
	- `Test.Speculate.Engine`: add three new wrappers for "theory and representatives";
	- `Test.Speculate.Expr.Core`: rename functions to `compareLexicographicallyBy` and `compareComplexityThenIndex`;
	- `Test.Speculate.Function`: add `Function.A10` and `A100` and `A1000`;
	- `Test.Speculate.Reason`: export `isRootNormal` and `isRootNormalE`;
* add trilean benchmark;
* improve order tests.


Earlier versions
----------------

Please refer to the [git commit history].

[git commit history]: https://github.com/rudymatela/speculate/commits/master

[Speculate]:        https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html
[`Test.Speculate`]: https://hackage.haskell.org/package/speculate/docs/Test-Speculate.html
[LeanCheck]:        https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html
[express]:          https://hackage.haskell.org/package/express
