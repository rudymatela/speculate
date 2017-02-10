module Test.Speculate.Expr.Canon
  ( canonicalize
  , canonicalizeWith
  , canonicalWith
  )
where

import Test.Speculate.Expr.Core
import Test.Speculate.Expr.Match
import Test.Speculate.Expr.TypeInfo
import Data.List ((\\))

-- | Canonicalize variable names in an expression.
--
-- > canonicalize (x + y) = (x + y)
-- > canonicalize (y + x) = (x + y)
-- > canonicalize (y + (z + x)) = (x + (y + z))
-- > canonicalize ((w + z) + (z + x)) = ((x + y) + (y + z))
-- > canonicalize (y + abs y) = (x + abs x)
-- > canonicalize ((y + x) == (x + y)) = ((x + y) == (y + x))
canonicalizeWith :: Instances -> Expr -> Expr
canonicalizeWith ti e = e `assigning` ((\(t,n,n') -> (n,Var n' t)) `map` cr [] e)
  where
  cr :: [(TypeRep,String,String)] -> Expr -> [(TypeRep,String,String)]
  cr bs (e1 :$ e2) = cr (cr bs e1) e2
  cr bs (Var n t)
    | any (\(t',n',_) -> t == t' && n == n') bs = bs
    | otherwise = (t,n,head $ names ti t \\ map (\(_,_,n) -> n) bs):bs
  cr bs _ = bs

canonicalize :: Expr -> Expr
canonicalize = canonicalizeWith preludeInstances

canonicalWith :: Instances -> Expr -> Bool
canonicalWith ti e = canonicalizeWith ti e == e
