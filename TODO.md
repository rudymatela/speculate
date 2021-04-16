TODO
====

A list of things to do for Speculate.

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
