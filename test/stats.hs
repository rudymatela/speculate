-- Test library
import Test

-- Functions under test
import Test.Speculate.Reason
import Test.Speculate.Utils

main :: IO ()
main = do
  n <- getMaxTestsFromArgs 10000

  putStrLn "typ (e :: Expr)"
  reportCountsBy typ            (take n list :: [Expr])

  putStrLn "size (e :: Expr)"
  reportCountsBy sizeInterval (take n list :: [Expr])

  putStrLn "typ (e1 :: Expr) == typ (e2 :: Expr)"
  reportCountsBy equalTypes (take n list)

  putStrLn "typ (e1 :: Expr) == typ (e2 :: Expr)"
  reportCountsBy equalTypes . map unSameTypeE . take n $ list

  putStrLn "okThy thy"
  reportCountsBy (show . okThy) (take n list)

  putStrLn "length (equations thy)"
  reportCountsBy (show . length . equations) (take n list)

  putStrLn "length (rules thy)"
  reportCountsBy (show . length . rules) (take n list)

  putStrLn "length (equations thy + rules thy)"
  reportCountsBy (show . length . (\thy -> rules thy ++ equations thy)) (take n list)

sizeInterval :: Expr -> (Int,Int)
sizeInterval e = (l, l+2) where l = (size e `div` 3) * 3

equalTypes :: (Expr,Expr) -> String
equalTypes (e1,e2) | typ e1 == typ e2 = show (typ e1)
                   | otherwise        = "invalid"
