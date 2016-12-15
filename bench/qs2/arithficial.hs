import QuickSpec

f :: Num a => a -> a -> a
f x y = x*17+23

g :: Num a => a -> a -> a
g x y = 42

h :: Num a => a -> a -> a
h x y = y*13+19

main =
  quickSpec
    signature
      { maxTermSize = Just 5
      , constants =
          [ constant "0" (0 :: Int)
          , constant "id" (id :: Int -> Int)
          , constant "+" ((+) :: Int -> Int -> Int)
          , constant "f" (f :: Int -> Int -> Int)
          , constant "g" (g :: Int -> Int -> Int)
          , constant "h" (h :: Int -> Int -> Int)
          ]
      }
