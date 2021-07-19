Changelog for Speculate
=======================

upcoming
--------

* add this changelog


v0.4.10
-------

* no changes in the actual Speculate library
* cleanup build files
* remove uneeded typeable derivations on examples and tests


v0.4.8
------

* no changes in the actual Speculate library
* refactor build scripts
* use GitHub Workflows as the CI
* fix compilation of some examples under the new LeanCheck


v0.4.6
------

* `Test.Speculate`: export `reifyName`;
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

Please refer to the git commit history.
