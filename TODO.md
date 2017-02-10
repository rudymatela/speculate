TODO
====

A non-exhaustive list of things TODO for Speculate

Warning: I tend to ramble...


current
-------

* increase default limit for semi-equations?

* check Eq and Ord instances before running:
  including consistency between them.
  refuse to run if errors are found

* review README file.

* derive `tiers` using speculate itself.  Use provided constructors.
  Maybe a new field in args?



later
-----

* Implement expand by expanding tiers (more robust and flexible).  It
  will allow extraction of constant values from tiers.  This will also make it
  easy to amend a Thy: do theorization; add a bunch of atoms; do it again;

* make regex work on qs1 and qs2.


stranger things
---------------

* after adding:

    constant "/=" $ (/=) -:> integer

  to `backgroundConstants`, these:

    (q == negate q) == False
       (q == q + r) == False

  along with a handful of other strange laws appear.
  Find out why and remove them.


* see commit `f7b323a`, why does the following equation disappears after
  requiring a minimum number of tests to pass?

    x /= y ==>        delete y (insert x Null) == insert x Null

  The precondition should hold most of the time, so, a minimum number of
  tests should not discard it.


redundancy to remove
--------------------

* on `eg/list`:

  xs <= head xs:tail xs

  this follows from xs == head xs:tail xs
  but is actually not true in general (empty xs)
  so maybe just test the reverse and discard if it holds?

* remove redundancy on taut example:

	taut q ==> subst n (taut q) p == subst n True p

  pruning principle:
  1. `genericMatch LHS RHS = [(taut q, True)]`
  2. `equivalent thy (taut q) (taut q == True)`

* remove the following redundant laws on insertsort:

	ordered (ys ++ xs) ==>       ys ++ sort xs == sort (xs ++ ys)
	ordered (ys ++ xs) ==>       sort ys ++ xs == sort (xs ++ ys)
	ordered (ys ++ xs) ==>  sort ys ++ sort xs == sort (xs ++ ys)

  implied by `ordered (sort xs) == True` *and* `sort (xs++ys) == sort (ys++xs)`

* On `./eg/digraphs -s6`, I get
  `False == isNode x a ==>  succs x a == preds x a`
  (and other related equations.) A more general version wouldn't be
  `False == isNode x a ==>  succs x a == []`?

  I checked on ghci, it does hold for 30000 tests, so the library isn't buggy.

  Maybe the issue is that `== []` is redundant and discarded?

  There are lots of other redundant equations there.  Maybe those are related
  to the planned genericMatch pruning principle?  (see a bit above)


Later Later
-----------

* add maximum commutative size limit?

* improve error message for missing typeInfo.  Maybe add full suggestion.

* include Colin's list module example

* (for performance and interface): actually compute what happens with
  undefined values.  e.g.: head [] == undefined.  This will/may make things
  faster as we can prune foo (head []) or head [] ++ head [], which are also
  undefined.

* (for performance) note that variable assignments form a lattice.  So I only
  need to test stuff from upper if the lower is true.  Of course, testing is
  the expensive thing.  But it does not pay off to test x + y = z + w before
  testing x + y = y + x.  The second needs to hold for the first to hold.  And,
  it will be far more common!

* (for performance) hardcode laws about `<=`, `<` and `==`?  nah!

* (for interface) I actually do not need to provide 0-argument constants in the
  background algebra.  Since I am using an enumerative strategy, I can actually
  enumerate those from the TypeInfo.  This way, background will look nicer,
  with less functions and values.  Computing the size of values and expressions
  may be a problem.

* (for interface) make dwoList, where the order between expressions is given by
  the order in which they appear in a list.  Note this *cannot* be composed
  with a lexicographical order (as it could break transitivity, I think).
  Better raise an error in case a symbol is not in the list.  On second thought,
  I think it can be composed.  Just make everything in the list "smaller" than
  whatever is not in the list.

* (for performance) This one is a maybe.  When generating preconditions, do not
  consider (<=), only consider (<), because I can always weaken the
  precondition later.  (update: nah!)

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

* require _some_ cases of `e1 == e2` before considering `ce ==> e1 == e2`.
  10% by default?


### Properties I want

From:
	1. `             i <= abs i   `
	2. `negate (abs i) <= negate i`
Remove 2 because of:
	3.  `x < y  ==>  negate y < negate x`

In the list example, I want:
	* ` x <  y    ==>      x:xs <  y:ys`
	* `xs <  ys   ==>      x:xs <  x:ys`

In the graph, instead of:
	1. `isNode x (addNode y emptyDigraph) == isNode y (addNode x emptyDigraph)`
have:
    2. `x /= y ==> isNode x (addNode y emptyDigraph) == False`
