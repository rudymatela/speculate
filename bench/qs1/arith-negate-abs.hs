import Test.QuickSpec

type I = Int

main =
  quickSpec
    [ ["x", "y", "z"] `vars` (undefined :: I)
    , "0" `fun0` (0   :: I)
    , "1" `fun0` (1   :: I)
    , "+" `fun2` ((+) :: I -> I -> I)
    , "*" `fun2` ((*) :: I -> I -> I)
    , "id"     `fun1` (id     :: I -> I)
    , "abs"    `fun1` (abs    :: I -> I)
    , "negate" `fun1` (negate :: I -> I)
    ]
