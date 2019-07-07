TODO
====

A list of things to do for Speculate.

* release new Speculate after releasing Haexpress
  (which will happen after I have a version of Extrapolate that uses Haexpress)

* simplify "test" handling.  Do like in LeanCheck and Extrapolate.
  instead of having to run `make tests/test-something.test`,
  run with `make test/something.run`.

* simpilfy "bench" handing.  Do like in LeanCheck and Extrapolate.

* (code readability) review and document code

* (code readability) rename semi to inqualities everywhere.

* (interface) print errors on stderr, not on stdout

* (performance) note that variable assignments form a lattice.  So I only
  need to test stuff from upper if the lower is true.  Of course, testing is
  the expensive thing.  But it does not pay off to test x + y = z + w before
  testing x + y = y + x.  The second needs to hold for the first to hold.  And,
  it will be far more common!

* (interface) I actually do not need to provide 0-argument constants in the
  background algebra.  Since I am using an enumerative strategy, I can actually
  enumerate those from `Instances`.  This way, background will look nicer,
  with less functions and values.  Computing the size of values and expressions
  may be a problem.

* (performance) Improve the performance of KBCompletion.
  In the process of generating equivalences, the slowest function is complete,
  accounting for 88 percent of runtime.  Of that:
  - complete     -- 88%
  - deduce       -- 79%
  - normalizedCP -- 78%
  - normalize    -- 68%
  I don't think normalizedCriticalPairs / normalize can be optimized any
  further.  Maybe the problem comes with complete itself, that should deduce
  less often or even maybe interleave steps more often.  Maybe adding
  normalizedCriticalPairs as soon as I add a rule?  Running deduce twice less
  often does not help, as other steps take a bit over and deduce still accounts
  for a high percentage (let's say 60%).  Possible fixes:
  - implement deduce2, simplify2, compose2 and collapse2 from unfailing
    completion
  - finish groundJoinable from "Ordered Rewriting and Confluence" by
    adding one last condition

* (performance and interface): actually compute what happens with
  undefined values.  e.g.: head [] == undefined.  This will/may make things
  faster as we can prune foo (head []) or head [] ++ head [], which are also
  undefined.
