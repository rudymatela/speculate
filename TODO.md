TODO
====

A non-exhaustive list of things TODO for Speculate

Warning: I tend to ramble...


current
-------

* rename atoms to constants

* remove redundant equation on numbool -s4

* Implement expand by expanding tiers.  This is more robust and flexible.  It
  will allow extraction of contant values from tiers.  This will also make it
  easy to amend a Thy: do theorization; add a bunch of atoms; do it again.


Later Later
-----------

* include Colin's list module example

* fix wrong laws that appear on `./eg/digraphs -s6`

* Colin: why _not_, for example:
  isPath x y (subgraph zs a) ==> isPath x y a
  (size 6)

* (for performance and interface): actually compute what happens with
  undefined values.  e.g.: head [] == undefined.  This will/may make things
  faster as we can prune foo (head []) or head [] ++ head [], which are also
  undefined.

* add regex example, for Saloomaa inference, we only need conditional
  equations.  For Kozen, we need conditional inequations.

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


* Rename "Speculate" to "Test.Speculate"

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
