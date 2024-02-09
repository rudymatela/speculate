TODO
====

A list of things to do for Speculate.

* update CI to include newer GHCs

* add a couple of tests to `groundJoinable`

* implement missing `groundJoinable` criterium

* (code readability) review and document code

* (code readability) rename semi to inqualities everywhere.

* (interface) print errors on stderr, not on stdout

* (bug) the following now hangs on equivalencesBetween

        make eg/arith && long ./eg/arith -TSs7

  There are `4213394` canonicalVariations of `((_ + _) * _ + (_ + _) * _ :: Int,(_ + _) * _ + (_ + _) * _ :: Int)`.  Testing them all is perhaps not worth it.

* (performance) note that variable assignments form a lattice.  So I only
  need to test stuff from upper if the lower is true.  Of course, testing is
  the expensive thing.  But it does not pay off to test x + y = z + w before
  testing x + y = y + x.  The second needs to hold for the first to hold.  And,
  it will be far more common!
