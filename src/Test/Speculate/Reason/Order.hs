-- |
-- Module      : Test.Speculate.Reason.Order
-- Copyright   : (c) 2016-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part o Speculate.
--
-- Orders for term rewriting and completion.
module Test.Speculate.Reason.Order
  ( (|>|)
  , (>|)
  , (|>)
  , kboBy
  , dwoBy
  , weight
  , weightExcept
  , gtExcept
  , funny
  , serious
  )
where

import Test.Speculate.Expr
import Test.Speculate.Utils (nubMerge)

-- | Greater than or equal number of occurences of each variable
(>=\/) :: Expr -> Expr -> Bool
e1 >=\/ e2 = all (\e -> countVar e e1 >= countVar e e2)
                 (nubVars e1 `nubMerge` nubVars e2)
  where
  countVar e = length . filter (== e) . vars


-- | Strict order between expressions as defined in TRAAT p103.
--
-- > s > t iff |s| > |t| and , for all x in V, |s|_x > |t|_x
--
-- This is perhaps the simplest order that can be used with KBC.
(|>|) :: Expr -> Expr -> Bool
e1 |>| e2 = size e1 > size e2
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
  ef :$ (eg :$ ex) `fn` ey | isVar ey && ef == eg = fn (eg :$ ex) ey
  ef@(Value _ _) :$ ex `fn` ey | isVar ex && isVar ey && ex == ey = True
  _ `fn` _ = False
  e1 `fg` e2 =
    case (unfoldApp e1, unfoldApp e2) of
      -- do I really need the _:_ instead of just _?
      -- do I really need to restrict to functional values?
      (ef:(_:_),eg:(_:_)) | isConst ef && isConst eg -> ef ->- eg
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
  w e | isVar e   = 1
      | otherwise = case arity e of
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
  w e | isVar e   = 1
      | otherwise = case arity e of
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
--
-- This is not a simplification order on 'funny' 'Expr's.
(|>) :: Expr -> Expr -> Bool
(|>) = dwoBy (>)
infix 4 |>

-- | Dershowitz reduction order as defined in TRAAT
--
-- @|>@ a "D" for Dershowitz
--
-- This is not a simplification order on 'funny' 'Expr's.
dwoBy :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Bool
dwoBy (>) = (|>)
  where
  e1 |> e2 | isVar e2 && e2 `elem` nubVars e1 && e1 /= e2 = True
  e1 |> e2 = any (|>= e2) xs
          || (notVar f && notVar g && f >  g && all (e1 |>) ys)
          || (notVar f && notVar g && f == g && all (e1 |>) ys
              && case dropEq xs ys of
                   (x:_,y:_) -> x |> y
                   _         -> False)
    where
    (f:xs) = unfoldApp e1
    (g:ys) = unfoldApp e2
    notVar = not . isVar
    e1 |>= e2 = e1 == e2
             || e1 |> e2

-- | Returns 'True' when an 'Expr'ession has either
--   a functional type or functional 'var'iables.
--
-- > funny xx        =  False
-- > funny one       =  False
-- > funny idE       =  True
-- > funny (ff one)  =  True
funny :: Expr -> Bool
funny e  =  any (isFunTy . typ) $ e:vars e

-- | Returns 'True' when an 'Expr'ession has
--   no functional return type and no functional 'var'iables.
--
-- > serious xx        =  True
-- > serious one       =  True
-- > serious idE       =  False
-- > serious (ff one)  =  False
serious :: Expr -> Bool
serious  =  not . funny


--- Misc Utilities ---

dropEq :: Eq a => [a] -> [a] -> ([a],[a])
dropEq (x:xs) (y:ys) | x == y = dropEq xs ys
dropEq xs ys = (xs,ys)
