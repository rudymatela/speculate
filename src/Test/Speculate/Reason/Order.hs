module Test.Speculate.Reason.Order
  ( (|>|)
  , (>|)
  , (|>)
  , kboBy
  , dwoBy
  , weight
  , weightExcept
  , gtExcept
  )
where

import Test.Speculate.Expr
import Test.Speculate.Utils (nubMerge)

-- | Greater than or equal number of occurences of each variable
(>=\/) :: Expr -> Expr -> Bool
e1 >=\/ e2 = all (\(t,n) -> countVar t n e1 >= countVar t n e2)
                 (vars e1 `nubMerge` vars e2)


-- | Strict order between expressions as defined in TRAAT p103.
--
-- > s > t iff |s| > |t| and , for all x in V, |s|_x > |t|_x
--
-- This is perhaps the simplest order that can be used with KBC.
(|>|) :: Expr -> Expr -> Bool
e1 |>| e2 = lengthE e1 > lengthE e2
         && e1 >=\/ e2
infix 4 |>|


-- | Strict order between expressions loosely as defined in TRAAT p124 (KBO)
--
-- Reversed K @>|@ for Knuth, sorry Bendix.
(>|) :: Expr -> Expr -> Bool
(>|) = kboBy weight (>)
infix 4 >|

kboBy :: (Expr -> Int) -> (Expr -> Expr -> Bool) -> Expr -> Expr -> Bool
kboBy w (->-) e1 e2 = e1 >=\/ e2
                   && ( w e1 >  w e2
                     || w e1 == w e2 && ( e1 `fn` e2 -- f (f x) > x
                                       || e1 `fg` e2 -- f x     > g y     if f > g
                                       || e1 `ff` e2 -- f x y z > f x w v if y > w
                                        )
                      )
  where
  ef :$ (eg :$ ex)               `fn` ey@(Var _ _) | ef == eg = fn (eg :$ ex) ey
  ef@(Constant _ _) :$ ex@(Var _ _) `fn` ey@(Var _ _) | ex == ey = True
  _ `fn` _ = False
  e1 `fg` e2 =
    case (unfoldApp e1, unfoldApp e2) of
      -- do I really need the _:_ instead of just _?
      -- do I really need to restrict to functional values?
      (ef@(Constant _ _):(_:_),eg@(Constant _ _):(_:_)) -> ef ->- eg
      _ -> False
  e1 `ff` e2 =
    case (unfoldApp e1, unfoldApp e2) of
      -- Not restricting to functional values.
      -- Since we are making an equality comparison,
      -- this hopefully will be strict enough not bo break KBO.
      (f:xs,g:ys) -> f == g
                  && length xs == length ys
                  && case dropEq xs ys of
                       (x:_,y:_) -> x >=\/ y
                       _ -> False
      _           -> False

-- | Weight function for kboBy:
--
-- * Variables         weigh 1
-- * Nullary functions weigh 1  (a.k.a. constants)
-- * N-ary   functions weigh 0
-- * Unary   functions weigh 1
--
-- This is the weight when using '>|'.
weight :: Expr -> Int
weight = w
  where
  w (e1 :$ e2) = weight e1 + weight e2
  w (Var _ _)  = 1
  w e = case arity e of
          0 -> 1
          1 -> 1
          _ -> 0

-- | Weight function for kboBy:
--
-- * Variables         weigh 1
-- * Nullary functions weigh 1  (a.k.a. constants)
-- * N-ary   functions weigh 0
-- * Unary   functions weigh 1 except for the one given as argument
weightExcept :: Expr -> Expr -> Int
weightExcept f0 = w
  where
  w (e1 :$ e2) = w e1 + w e2
  w (Var _ _)  = 1
  w e = case arity e of
          0 -> 1
          1 -> if e == f0 then 0 else 1
          _ -> 0

-- | To be used alongside weightExcept
gtExcept :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Expr -> Bool
gtExcept (>) f0 e1 e2 | e2 == f0  = False -- nothing can be greater than f0
                      | e1 == f0  = True  -- f0 is greater than everything
                      | otherwise = e1 > e2 -- compare normally

-- Note this default Dershowitz order can sometimes be weird:
--
-- > x - y |> x + negate y  -- as (-) > (+)
-- > negate x + y |> negate (x + negate y)  -- as (+) > negate, as I->I->I > I->I
(|>) :: Expr -> Expr -> Bool
(|>) = dwoBy (>)
infix 4 |>

-- | Dershowitz reduction order as defined in TRAAT
--
-- @|>@ a "D" for Dershowitz
dwoBy :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Bool
dwoBy (>) = (|>)
  where
  e1 |> e2@(Var n t) | (t,n) `elem` vars e1 && e1 /= e2 = True
  e1 |> e2 = any (|>= e2) xs
          || (notVar f && notVar g && f >  g && all (e1 |>) ys)
          || (notVar f && notVar g && f == g && all (e1 |>) ys
              && case dropEq xs ys of
                   (x:_,y:_) -> x |> y
                   _         -> False)
    where
    (f:xs) = unfoldApp e1
    (g:ys) = unfoldApp e2
    notVar (Var _ _) = False
    notVar _         = True
    e1 |>= e2 = e1 == e2
             || e1 |> e2



--- Misc Utilities ---

dropEq :: Eq a => [a] -> [a] -> ([a],[a])
dropEq (x:xs) (y:ys) | x == y = dropEq xs ys
dropEq xs ys = (xs,ys)