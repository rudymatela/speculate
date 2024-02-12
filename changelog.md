Changelog for Speculate
=======================


v0.4.18 (February 2024)
-----------------------

* `Test.Speculate.Expr.Ground`: add `constify` and `constifications`
* `Reason`: fix `groundJoinable` with the last criterium
* `Reason`: use `unificationsC` to unify
* minor improvements in the code
* rework test scripts and `Makefile`
* remove stale tests


v0.4.16 (February 2024)
-----------------------

* no changes in API
* fix compatibility with [express] >= v1.0.16
* bump [LeanCheck] requirement to >= 1.0.0
* improve tests of Speculate itself
* add release dates on this changelog


v0.4.14 (September 2021)
------------------------

* `Test.Speculate.Reason`: add `doubleCheck`;
* add `lowtests` benchmark;
* bump [express] requirement to v1.0.0;
* fix parallel compilation when using the Makefile.


v0.4.12 (July 2021)
-------------------

* bump [express] requirement to v0.2.0
* add this changelog


v0.4.10 (June 2021)
-------------------

* no changes in the actual [Speculate] library
* cleanup build files
* remove uneeded typeable derivations on examples and tests


v0.4.8 (June 2021)
------------------

* no changes in the actual [Speculate] library
* refactor build scripts
* use GitHub Workflows as the CI
* fix compilation of some examples under the new [LeanCheck]


v0.4.6 (April 2021)
-------------------

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
