import Test.Speculate

-- TODO: shouldn't this report the law x == y?
-- currently (2017-09-13) it reports no laws!
main :: IO ()
main = speculate args
  { constants =
      [ constant "()" ()
      , constant "id" (id :: () -> ())
      ]
  }
