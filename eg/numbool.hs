import Speculate
import Test.LeanCheck ((==>))

main :: IO ()
main = speculate args
  { atoms =
      [ showConstant False
      , showConstant True
      , constant "not" not
    --, constant "&&"  (&&)
    --, constant "||"  (||)

      , showConstant (0::Int)
      , showConstant (1::Int)
      , constant "+"   $ (+)  -:> int
      , constant "<="  $ (<=) -:> int
      , constant "<"   $ (<)  -:> int
      , constant "=="  $ (==) -:> int
      , constant "abs" $ abs  -:> int
      ]
  }
