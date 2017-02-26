import Test.QuickSpec

main =
  quickSpec
    [ ["x", "y", "z"]    `vars` (undefined :: Int)
    , ["xs", "ys", "zs"] `vars` (undefined :: [Int])
    , "[]" `fun0` ([]   :: [Int])
    , ":"  `fun2` ((:)  :: Int -> [Int] -> [Int])
    , "++" `fun2` ((++) :: [Int] -> [Int] -> [Int])
    , "head" `fun1` (head :: [Int] -> Int)
    , "tail" `fun1` (tail :: [Int] -> [Int])
    ]
