import QuickSpec

main = quickSpec signature
  { maxTermSize = Just 9
  , maxTermDepth = Nothing -- Just 5
  , maxTests = Just 500
  , constants =
      [ constant "0"   (0   :: Int)
      , constant "1"   (1   :: Int)
      , constant "True" (True :: Bool)
      , constant "+"   ((+) :: Int -> Int -> Int)
      , constant "id"  (id  :: Int -> Int)
      , constant "abs" (abs :: Int -> Int)
      , constant "<="  ((<=) :: Int -> Int -> Bool)
      ]
  }
