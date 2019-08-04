TODO
====

A list of things to do for Speculate.

* simplify "test" handling.  Do like in LeanCheck and Extrapolate.
  instead of having to run `make tests/test-something.test`,
  run with `make test/something.run`.

* simpilfy "bench" handing.  Do like in LeanCheck and Extrapolate.

* improve performance of the Reason module:
  when listing rewrites of a given expression,
  I can use a custom data structure that computes
  matches all at once while traversing an expression.

* (code readability) review and document code

* (code readability) rename semi to inqualities everywhere.

* (interface) print errors on stderr, not on stdout

* (performance) note that variable assignments form a lattice.  So I only
  need to test stuff from upper if the lower is true.  Of course, testing is
  the expensive thing.  But it does not pay off to test x + y = z + w before
  testing x + y = y + x.  The second needs to hold for the first to hold.  And,
  it will be far more common!

* (performance and interface): actually compute what happens with
  undefined values.  e.g.: head [] == undefined.  This will/may make things
  faster as we can prune foo (head []) or head [] ++ head [], which are also
  undefined.
