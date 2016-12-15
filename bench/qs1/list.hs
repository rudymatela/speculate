import Test.QuickSpec

main =
  quickSpec
    [ ["x", "y", "z"]    `vars` (undefined :: Int)
    , ["xs", "ys", "zs"] `vars` (undefined :: [Int])
    , "[]" `fun0` ([]   :: [Int])
    , ":"  `fun2` ((:)  :: Int -> [Int] -> [Int])
    , "++" `fun2` ((++) :: [Int] -> [Int] -> [Int])
    ]
