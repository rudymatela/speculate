import QuickSpec

main = quickSpec signature
  { maxTermSize = Just 7
  , maxTermDepth = Just 4
  , constants =
      [ constant "0"   (0   :: Int)
      , constant "1"   (1   :: Int)
      , constant "+"   ((+) :: Int -> Int -> Int)
      , constant "id"  (id  :: Int -> Int)
      , constant "abs" (abs :: Int -> Int)
      ]
  }
