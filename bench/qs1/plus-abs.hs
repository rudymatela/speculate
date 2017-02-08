import Test.QuickSpec

main = quickSpec
  [ withSize  7
  , withDepth 4
  , ["x", "y", "z"] `vars` (undefined :: Int)
  , "0"   `fun0` (0   :: Int)
  , "1"   `fun0` (1   :: Int)
  , "+"   `fun2` ((+) :: Int -> Int -> Int)
  , "id"  `fun1` (id  :: Int -> Int)
  , "abs" `fun1` (abs :: Int -> Int)
  ]
