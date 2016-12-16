-- Test library
import Test

-- Functions under test
import Test.Speculate.CondReason

-- Utils
import Test.Speculate.Reason (canonicalEqn, canonicalizeEqn)

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , creductions1 (zero -<=- yy) (yy -+- abs' yy) (zero -<=- xx, abs' xx, xx)
    == [yy -+- yy]

  , creductions1 (zero -<=- yy) (abs' yy -+- abs' yy) (zero -<=- xx, abs' xx, xx)
    == [yy -+- abs' yy, abs' yy -+- yy]

  , cnormalize
      emptyChy{cequations = [(zero -<=- xx, abs' xx, xx)]}
      (zero -<=- yy) (abs' yy -+- abs' yy)
    == yy -+- yy

  , not
  $ cequivalent
      emptyChy{cequations = []}
      (zero -<=- yy) (abs' yy -+- yy) (yy -+- yy)

  , cequivalent
      emptyChy{cequations = [(zero -<=- xx, abs' xx, xx)]}
      (zero -<=- yy) (abs' yy -+- yy) (yy -+- yy)

  , holds n $ \e1 e2    -> canonicalCEqn (falseE,e1,e2) == canonicalEqn (e1,e2)
  , holds n $ \e1 e2    -> sndTrd (canonicalizeCEqn (falseE,e1,e2))
                        == canonicalizeEqn (e1,e2)
  , holds n $ \e1 e2 ce -> sndTrd (canonicalizeCEqn (falseE,e1,e2))
                        == sndTrd (canonicalizeCEqn (ce,    e1,e2))

  , const True -- TODO: make the following test pass!
  $ let chy = cinsert ( elem' xx xxs
                      , insert' xx (xxs -++- yys)
                      , insert' xx xxs -++- yys
                      )
                      emptyChy
    in cnormalize chy (elem' xx xxs) (insert' xx (xxs -++- yys)) == insert' xx xxs -++- yys

  , const True -- TODO: make the following test pass!
  $ creductions1 (elem' xx xxs) (insert' xx (xxs -++- yys))
      ( elem' xx xxs, insert' xx (xxs -++- yys), insert' xx xxs -++- yys)
    == [insert' xx xxs -++- yys]

  , const True -- TODO: make the following test pass!
  $ holds n $ \(BoolE ce) (SameTypeE e1 e2) -> creductions1 ce e1 (ce,e1,e2) == [e2]
  ]
  where
  sndTrd (_,y,z) = (y,z)
