import Test.QuickSpec

f :: Num a => a -> a -> a
f x y = x*17+23

g :: Num a => a -> a -> a
g x y = 42

h :: Num a => a -> a -> a
h x y = y*13+19

type I = Int

main =
  quickSpec
    [ ["x", "y", "z", "w"] `vars` (undefined :: I)
    , "0"  `fun0` (0   :: I)
    , "id" `fun1` (id :: I -> I)
    , "+"  `fun2` ((+) :: I -> I -> I)
    , "f"  `fun2` (f :: I -> I -> I)
    , "g"  `fun2` (g :: I -> I -> I)
    , "h"  `fun2` (h :: I -> I -> I)
    ]
